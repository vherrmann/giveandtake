module GiveAndTake.Handlers.Friends where

import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils (getFriends)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils (getUTCTime)
import Servant (ServerError (..), type (:<|>) (..))
import Servant qualified as S

getFriendsH :: (HasHandler m) => UserId -> m [WithKey User UserPublic]
getFriendsH = getFriends

friendsExist :: (HasHandler m) => UserId -> UserId -> m Bool
friendsExist user1Id user2Id = do
  exists1 <- runDB $ P.existsBy (UniqueFriends user1Id user2Id)
  exists2 <- runDB $ P.existsBy (UniqueFriends user2Id user1Id)
  pure $ exists1 || exists2

friendRequestExist :: (HasHandler m) => UserId -> UserId -> m Bool
friendRequestExist user1Id user2Id = do
  exists1 <- runDB $ P.existsBy (UniqueFriendRequest user1Id user2Id)
  exists2 <- runDB $ P.existsBy (UniqueFriendRequest user2Id user1Id)
  pure $ exists1 || exists2

deleteFriendsH :: (HasHandler m) => UserId -> UserId -> m ()
deleteFriendsH userId friendId = do
  existP <- friendsExist userId friendId
  unless existP $ throwError S.err409{errBody = "Not friends."}
  runDB $ P.deleteBy $ UniqueFriends userId friendId
  runDB $ P.deleteBy $ UniqueFriends friendId userId

getFriendRequests :: (HasHandler m) => UserId -> m FriendsRequestGetResponse
getFriendRequests userId = do
  requestsToYouEnt <-
    selectByKey @User . fmap (.val.from)
      =<< runDB (P.selectList [FriendRequestTo P.==. userId] [])
  requestsFromYouEnt <-
    selectByKey @User . fmap (.val.to)
      =<< runDB (P.selectList [FriendRequestFrom P.==. userId] [])
  let entToPubId ent = WithKey{key = ent.key, value = userToPublic ent.val}
      requestsToYou = entToPubId <$> requestsToYouEnt
      requestsFromYou = entToPubId <$> requestsFromYouEnt
  pure $ FriendsRequestGetResponse{requestsToYou, requestsFromYou}

-- TODO: notify friend
postFriendRequest :: (HasHandler m) => UserId -> UserId -> m ()
postFriendRequest userId friendId = do
  fExistP <- friendsExist userId friendId
  when fExistP $ throwError S.err409{errBody = "Already friends."}
  frExistP <- friendRequestExist userId friendId
  when frExistP $ throwError S.err409{errBody = "Friendship request already exists"}

  assureByKeySE @User userId
  assureByKeySE @User friendId

  ct <- getUTCTime
  runDB $ P.insert_ FriendRequest{from = userId, to = friendId, createdAt = ct}

-- TODO: notify friend
acceptFriendRequest :: (HasHandler m) => UserId -> UserId -> m ()
acceptFriendRequest userId friendId = do
  frExistP <- friendRequestExist userId friendId
  unless frExistP $ throwError S.err409{errBody = "Friendship request does not exist."}
  ct <- getUTCTime
  runDB $ P.insert_ Friends{user1 = userId, user2 = friendId, createdAt = ct}
  runDB $ P.deleteBy $ UniqueFriendRequest friendId userId

-- TODO: notify friend?
rejectFriendRequest :: (HasHandler m) => UserId -> UserId -> m ()
rejectFriendRequest userId friendId = do
  frExistP <- friendRequestExist userId friendId
  unless frExistP $ throwError S.err409{errBody = "Friendship request does not exist."}
  runDB $ P.deleteBy $ UniqueFriendRequest friendId userId

friendsHandler :: P.Entity User -> RServer m FriendsApi
friendsHandler userEnt =
  let userId = userEnt.key
   in getFriendsH userId
        :<|> deleteFriendsH userId
        :<|> getFriendRequests userId
        :<|> postFriendRequest userId
        :<|> acceptFriendRequest userId
        :<|> rejectFriendRequest userId
