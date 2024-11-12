module GiveAndTake.Handlers.Auth where

import Control.Monad.Error.Class
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Database.Persist ((=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils (withValidateTokenSE)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import GiveAndTake.VerifyEmail (finishEmailVerification, startVerifyEmailJob)
import Servant (ServerError (..), err409, err500, type (:<|>) (..))
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
    else do
      withValidateTokenSE loginData.password user.passwordHash do
        -- IMPORTANT: The type of the third argument to acceptLogin has to match the type of the second argument of SA.Auth
        applyCookies <-
          maybeToMErr err500{errBody = "Server error while setting cookies."}
            =<< liftIO (SA.acceptLogin cookieSettings jwtSettings usrEntity)
        pure $
          applyCookies
            SuccessLoginResponse
              { userId = usrEntity.key
              , user = usrEntity.val
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

signupHandler :: (HasHandler m) => SignupData -> m JobId
signupHandler signupData = do
  -- just check, so that users don't fill in all the information without proper secret
  checkSecretWithUpdate []

  when (T.length signupData.name == 0) $
    throwError err409{errBody = "Name cannot be empty."}

  when (T.length signupData.name > 20) $
    throwError err409{errBody = "Name cannot be longer than 20 characters."}

  nameExists <- runDB $ P.existsBy (UniqueUserName signupData.name)
  when nameExists $
    throwError err409{errBody = "Name already exists."}
  emailExists <- runDB $ P.existsBy (UniqueUserEmail signupData.email)
  when emailExists $
    throwError err409{errBody = "Email already exists."}

  -- check secret again (so that it can't be used twice), update it as used
  checkSecretWithUpdate [AuthCodeUsed =. True]

  hash <- hashToken signupData.password
  ct <- getUTCTime

  userId <-
    runDB $
      insertUUID
        User
          { name = signupData.name
          , email = signupData.email
          , passwordHash = hash
          , createdAt = ct
          , fullyAuthenticated = False
          , avatar = Nothing
          }
  startVerifyEmailJob userId signupData.name signupData.email ECReasonSignup
 where
  checkSecretWithUpdate updates = do
    mSecret <- runDB $ updateGetBy (UniqueAuthCode signupData.secret) updates

    case mSecret of
      Nothing -> throwError S.err403{errBody = "Wrong secret"}
      Just secret -> when secret.val.used $ throwError S.err401{errBody = "Secret already used."}

startEmailVerificationH :: (HasHandler m) => EmailVerificationRequest -> m ()
startEmailVerificationH req = do
  mUserEnt <- runDB $ P.getBy (UniqueUserEmail req.email)
  case mUserEnt of
    Nothing -> logInfo [fmt|Email verification requested for non-existing user: {req.email}|]
    -- NOTE: we are catching all the errors so that the personal information of the users isn't leaked
    -- FIXME: log errors
    Just userEnt ->
      void (startVerifyEmailJob userEnt.key userEnt.val.name req.email ECReasonSignup)
        `catchError` \(e :: ServerError) -> logInfo [fmt|Error during email verification: {show @Text e}|]

finishEmailVerificationH :: (HasHandler m) => VerifyEmail -> m ()
finishEmailVerificationH = finishEmailVerification

checkHandler :: (HasHandler m) => SA.AuthResult (P.Entity User) -> m CheckResponse
checkHandler (SA.Authenticated user) = do
  upToDateUser <- getByKeySE user.key
  pure CheckResponse{user = upToDateUser, userId = user.key}
checkHandler failure = throwError err401{errBody = "Cookie authentication failed: " <> BL.pack (show failure)}

authHandler :: SA.CookieSettings -> SA.JWTSettings -> RServer m AuthApi
authHandler cs jwt =
  loginHandler cs jwt
    :<|> logoutHandler cs
    :<|> signupHandler
    :<|> (startEmailVerificationH :<|> finishEmailVerificationH)
    :<|> checkHandler
