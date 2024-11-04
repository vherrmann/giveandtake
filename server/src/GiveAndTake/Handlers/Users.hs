module GiveAndTake.Handlers.Users where

import Database.Persist ((==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.Api qualified as Api
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

getUserPostsH :: (HasHandler m) => Entity User -> UserUUID -> m [WithUUID ApiPost]
getUserPostsH userEnt requestedUserId = do
  checkIsFriendOrEq userEnt.key requestedUserId
  getUserApiPosts requestedUserId
