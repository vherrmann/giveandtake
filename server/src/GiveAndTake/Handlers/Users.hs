module GiveAndTake.Handlers.Users where

import Database.Persist ((=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils
import GiveAndTake.Media (deleteMedia, insertMedia, isImage)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils (hashToken)
import GiveAndTake.VerifyEmail (startVerifyEmailJob, verifyEmailOnSuccess)
import Servant ((:<|>) (..))
import Servant qualified as S
import Servant.Multipart qualified as SM

usersHandler :: Entity User -> RServer m UsersApi
usersHandler userEnt = getUserPublicH :<|> getUserPostsH userEnt :<|> userSettingsHandler userEnt

getUserPublicH :: (HasHandler m) => UserId -> m UserPublic
getUserPublicH userId = do
  user <- getByKeySE userId
  pure $ userToPublic user

getUserPostsH :: (HasHandler m) => Entity User -> UserId -> m [WithKey Post ApiPost]
getUserPostsH userEnt requestedUserId = do
  checkIsFriendOrEq userEnt.key requestedUserId
  getUserApiPosts requestedUserId

userSettingsHandler :: Entity User -> RServer m UserSettingsApi
userSettingsHandler userEnt =
  (getUserSettingsH userEnt :<|> updateUserSettingsH userEnt)
    :<|> changePasswordH userEnt
    :<|> changeEmailH userEnt
    :<|> changeAvatarH userEnt
    :<|> deleteAvatarH userEnt

getUserSettingsH :: Entity User -> RHandler m ApiUserSettings
getUserSettingsH userEnt = do
  pure $ ApiUserSettings{name = userEnt.val.name}

updateUserSettingsH :: Entity User -> ApiUserSettings -> RHandler m ()
-- We use record wildcards for warnings about unused fields
updateUserSettingsH userEnt (ApiUserSettings{..}) = do
  upToDateUser <- getByKeySE userEnt.key
  when (upToDateUser.name == name) $
    throwError S.err400{S.errBody = "Can't change to the same name."}
  usernameExp <- runDB $ P.existsBy (UniqueUserName name)
  when usernameExp $
    throwError S.err409{S.errBody = "Username already taken."}
  runDB $ P.update userEnt.key [UserName =. name]

changePasswordH :: Entity User -> ChangePassword -> RHandler m ()
changePasswordH userEnt changePwd = do
  upToDateUser <- getByKeySE userEnt.key
  withValidateTokenSE changePwd.oldPassword upToDateUser.passwordHash do
    newPasswordHash <- hashToken changePwd.newPassword
    runDB $ P.update userEnt.key [UserPasswordHash =. newPasswordHash]

changeEmailH :: Entity User -> ChangeEmailAddress -> RHandler m (Maybe JobId)
changeEmailH userEnt changeEmail =
  withValidateTokenSE changeEmail.password userEnt.val.passwordHash do
    mEmailConfEnt <- runDB $ P.selectFirst @_ @_ @EmailConfirm [#email ==. changeEmail.newEmail, #user ==. userEnt.key, #isConfirmed ==. True] []
    -- FIXME: require recent proof of ownership of the new email
    case mEmailConfEnt of
      Just emailConfEnt -> Nothing <$ verifyEmailOnSuccess emailConfEnt.val
      Nothing -> Just <$> startVerifyEmailJob userEnt.key userEnt.val.name changeEmail.newEmail ECReasonChangeEmail

deleteAvatar :: Entity User -> RHandler m ()
deleteAvatar userEnt = do
  upToDateUser <- getByKeySE userEnt.key
  runDB $ P.update userEnt.key [UserAvatar =. Nothing]
  whenLet upToDateUser.avatar \avatar ->
    deleteMedia [avatar]

changeAvatarH :: Entity User -> UploadAvatar -> RHandler m JobId
changeAvatarH userEnt uploadAvatar = do
  let file = uploadAvatar.file
  unless (isImage file) $ throwError S.err400{S.errBody = "Only images can be avatars."}
  deleteAvatar userEnt
  insertMedia userEnt MUReasonUserAvatar [file]

deleteAvatarH :: Entity User -> RHandler m ()
deleteAvatarH = deleteAvatar
