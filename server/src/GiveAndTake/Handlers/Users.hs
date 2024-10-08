module GiveAndTake.Handlers.Users where

import Database.Persist ((==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant ((:<|>) (..))

usersHandler :: Entity User -> RServer m UsersApi
usersHandler userEnt = getUserPublicH :<|> getUserPostsH userEnt

getUserPublicH :: (HasHandler m) => UserUUID -> m UserPublic
getUserPublicH userId = do
  user <- getByKeySE @User userId
  pure
    UserPublic
      { name = user.name
      , createdAt = user.createdAt
      }

getUserPostsH :: (HasHandler m) => Entity User -> UserUUID -> m [WithUUID Post]
getUserPostsH userEnt requestedUserId = do
  checkIsEqUser userEnt.key requestedUserId
  postList <- runDB $ P.selectList [PostUser ==. requestedUserId] [P.Desc PostCreatedAt]
  pure $ entityToWithUUID <$> postList
