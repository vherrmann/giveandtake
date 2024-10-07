module GiveAndTake.Handlers.Auth where

import Control.Monad.Except (MonadError (..))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.String (String)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID (UUID)
import Database.Persist ((+=.), (=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Email (Mail (..), sendMail)
import GiveAndTake.Handlers.Feed
import GiveAndTake.Notify
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Network.Mail.SMTP qualified as MS
import Servant (ServerError (..), err404, err406, err409, err500, type (:<|>) (..))
import Servant qualified as S
import Servant.Auth.Server qualified as SA
import Servant.Server (err401)

loginHandler ::
  (HasHandler m) =>
  SA.CookieSettings ->
  SA.JWTSettings ->
  LoginData ->
  m
    ( S.Headers
        '[ S.Header "Set-Cookie" SA.SetCookie
         , S.Header "Set-Cookie" SA.SetCookie
         ]
        SuccessLoginResponse
    )
loginHandler cookieSettings jwtSettings loginData = do
  -- Email adresses are defined to be unique, so we can just select the first member
  usrEntity <-
    maybeToMErr err401{errBody = "User not found."}
      =<< runDB (P.selectFirst [UserEmail ==. loginData.email] [])
  let user = usrEntity.entityVal
  if not user.fullyAuthenticated
    then throwError err401{errBody = "Not fully authenticated yet (E.g. email address has not been verified yet)."}
    else case validateTokenEither loginData.password user.passwordHash of
      Left errStr -> throwError err500{errBody = "Server error while checking hash: " <> BL.pack errStr}
      Right False -> throwError err401{errBody = "Wrong password."}
      Right True -> do
        -- IMPORTANT: The type of the third argument to acceptLogin has to match the type of the second argument of SA.Auth
        applyCookies <-
          maybeToMErr err500{errBody = "Server error while setting cookies."}
            =<< liftIO (SA.acceptLogin cookieSettings jwtSettings usrEntity)
        pure $
          applyCookies
            SuccessLoginResponse
              { userId = entityUKey usrEntity
              , user = P.entityVal usrEntity
              }

logoutHandler ::
  (HasHandler m) =>
  SA.CookieSettings ->
  m
    ( S.Headers
        '[ S.Header "Set-Cookie" SA.SetCookie
         , S.Header "Set-Cookie" SA.SetCookie
         ]
        S.NoContent
    )
logoutHandler cookieSettings = do
  -- Email adresses are defined to be unique, so we can just select the first member
  pure $ SA.clearSession cookieSettings S.NoContent

signupHandler :: (HasHandler m) => SignupData -> m ()
signupHandler signupData = do
  mSecret <- runDB $ updateGetBy (UniqueAuthCode signupData.secret) [AuthCodeUsed =. True]
  if fromMaybe False $ mSecret <&> (not . (.val.used))
    then runDB $ P.deleteBy (UniqueAuthCode signupData.secret)
    else throwError S.err403{errBody = "Wrong secret"}

  when (T.length signupData.name == 0) $
    throwError err409{errBody = "Name cannot be empty."}

  nameExists <- runDB $ P.existsBy (UniqueUserName signupData.name)
  when nameExists $
    throwError err409{errBody = "Name already exists."}
  emailExists <- runDB $ P.existsBy (UniqueUserEmail signupData.email)
  when emailExists $
    throwError err409{errBody = "Email already exists."}
  hash <- hashToken 12 signupData.password
  ct <- getUTCTime

  uuid <-
    runDB $
      insertUUID
        User
          { name = signupData.name
          , email = signupData.email
          , passwordHash = hash
          , createdAt = ct
          , fullyAuthenticated = False
          }
  sendVerificationEmail uuid signupData
  pure ()

-- FIXME: inform user about email verification
sendVerificationEmail :: (HasHandler m) => UUID -> SignupData -> m ()
sendVerificationEmail userUuid signupData = do
  -- check if email is already confirmed and if limit has been reached
  mEmailConf <- runDB $ P.getBy $ UniqueEmailConfirmUser userUuid
  case mEmailConf of
    Just emailConf -> do
      if emailConf.val.isConfirmed
        then throwError err409{errBody = "Email already confirmed."}
        else do
          when (emailConf.val.count >= 3) $
            throwError err409{errBody = "Email confirmation limit reached."}
    Nothing -> pure ()

  -- send email
  secret <- randomUrlToken
  uconfig :: UConfig <- askM
  let authority = uconfig.authority -- FIXME: Get from config
      url = [fmt|https://{authority}/verifyemail?secret={secret}&userId={userUuid}|] :: Text
      emailPlainText =
        [fmtTrim|
             Hello {signupData.name}!

             Please verify your email address for {uconfig.serviceName} by clicking on the following link:
             Do not click this link if you have not created an account at {uconfig.serviceName}.
             {url}
             |]
  sendMail
    Mail
      { to = [MS.Address (Just signupData.name) signupData.email]
      , subject = "Verify your email"
      , plainBody = emailPlainText
      , htmlBody = Nothing
      }
  curtime <- getUTCTime
  hash <- hashToken 12 secret
  runDB $
    P.upsert
      EmailConfirm
        { user = userUuid
        , isConfirmed = False
        , secretHash = hash
        , confirmedAt = Nothing
        , count = 0 -- FIXME: Use this to limit the number of tries
        , sentAt = curtime
        }
      [EmailConfirmCount +=. 1, EmailConfirmSentAt =. curtime]
  pure ()

finishSignup :: (HasHandler m) => WithUUID User -> m ()
finishSignup user = do
  uconfig :: UConfig <- askM
  token <- randomUrlToken
  ct <- getUTCTime
  void $ runDB $ insertUUID Feed{user = user.uuid, token, fType = MainFeed, createdAt = ct}
  let feedUrl = fUrl uconfig user.uuid token
  notify
    [user]
    NewNotif
      { title = [fmt|Welcome to {uconfig.serviceName}! Use the feed!|]
      , content = NotifWelcome (NotifWelcomeMsg user.value.name feedUrl)
      , prio = NPMedium
      }

updateFullyAuthenticated :: (HasHandler m) => UUID -> m ()
updateFullyAuthenticated userId = do
  mUser <- runDB $ P.get $ packKey @User userId
  case mUser of
    Nothing -> logWarn [fmt|User {userId} not found.|]
    Just user -> do
      unless user.fullyAuthenticated do
        mEmailConf <- runDB $ P.getBy $ UniqueEmailConfirmUser userId
        case mEmailConf of
          Nothing -> pure ()
          Just emailConf ->
            when emailConf.val.isConfirmed do
              finishSignup (WithUUID userId user)

              runDB $
                P.update (packKey @User userId) [UserFullyAuthenticated =. True]

verifyEmailHandler :: (HasHandler m) => VerifyEmail -> m ()
verifyEmailHandler (VerifyEmail{user = userId, secret = secret}) = do
  putStrLn @String "verify"

  -- we don't want two seperate calls too succeed
  let getEmailConf updatep =
        maybeToMErr err404{errBody = "Email confirmation not found."} =<< runDB do
          eConfirm <- P.getBy $ UniqueEmailConfirmUser userId
          when updatep $
            P.updateWhere
              [EmailConfirmUser ==. userId]
              [ EmailConfirmIsConfirmed =. True
              ]
          pure eConfirm

  eConfEnt <- getEmailConf False
  ct <- getUTCTime
  let eConf = P.entityVal eConfEnt

  when ((30 * 60) `addUTCTime` eConf.sentAt < ct) $ -- expire after 30 minutes
    throwError err409{errBody = "Email confirmation expired."}

  if validateToken secret eConf.secretHash
    then do
      curtime <- getUTCTime
      nextEConfEnt <- getEmailConf True
      if nextEConfEnt.val.isConfirmed
        then throwError err409{errBody = "Email confirmation already confirmed."}
        else do
          runDB $
            P.updateWhere
              [EmailConfirmUser ==. userId]
              [ EmailConfirmConfirmedAt =. Just curtime
              ]
          updateFullyAuthenticated userId
      pure ()
    else throwError err406{errBody = "Secret does not match."}

checkHandler :: (HasHandler m) => SA.AuthResult (P.Entity User) -> m CheckResponse
checkHandler (SA.Authenticated user) = pure CheckResponse{user = P.entityVal user, userId = entityUKey user}
checkHandler failure = throwError err401{errBody = "Cookie authentication failed: " <> BL.pack (show failure)}

authHandler :: SA.CookieSettings -> SA.JWTSettings -> RServer m AuthApi
authHandler cs jwt =
  loginHandler cs jwt
    :<|> logoutHandler cs
    :<|> signupHandler
    :<|> verifyEmailHandler
    :<|> checkHandler
