module GiveAndTake.Handlers.Users where

import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant ((:<|>) (..))

usersHandler :: Entity User -> RServer m UsersApi
usersHandler userEnt = getUserPublicH :<|> getUserPostsH userEnt

getUserPublicH :: (HasHandler m) => UserId -> m UserPublic
getUserPublicH userId = do
  user <- getByKeySE userId
  pure
    UserPublic
      { name = user.name
      , createdAt = user.createdAt
      }

getUserPostsH :: (HasHandler m) => Entity User -> UserId -> m [WithKey Post ApiPost]
getUserPostsH userEnt requestedUserId = do
  checkIsFriendOrEq userEnt.key requestedUserId
  getUserApiPosts requestedUserId
