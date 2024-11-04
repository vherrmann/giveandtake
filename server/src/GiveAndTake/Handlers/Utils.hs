{-# LANGUAGE AllowAmbiguousTypes #-}

module GiveAndTake.Handlers.Utils where

import Control.Monad.Error.Class (MonadError (..))
import Data.Text.Encoding qualified as T
import Database.Persist ((<-.), (==.))
import Database.Persist qualified as P
import GHC.Base (Symbol)
import GiveAndTake.Api
import GiveAndTake.Api qualified as Api
import GiveAndTake.DB
import GiveAndTake.DB.Types qualified as DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils (getUTCTime)
import Servant (err303)
import Servant qualified as S

getFriends :: (HasDBPool m, MonadIO m, MonadLogger m) => UserUUID -> m [WithUUID UserPublic]
getFriends uuid = do
  listFstIds <- runDB $ fmap (.val.user2) <$> P.selectList [FriendsUser1 ==. uuid] []
  listSndIds <- runDB $ fmap (.val.user1) <$> P.selectList [FriendsUser2 ==. uuid] []

  friends <- selectByKey @User (listFstIds <> listSndIds)
  pure $ friends <&> \user -> WithUUID user.key (userToPublic user.val)

areFriends :: (HasDBPool m, MonadIO m) => UserUUID -> UserUUID -> m Bool
areFriends user1 user2 = do
  friendsp <- runDB $ P.exists [FriendsUser1 ==. user1, FriendsUser2 ==. user2]
  friendspOtherWay <- runDB $ P.exists [FriendsUser1 ==. user2, FriendsUser2 ==. user1]

  pure $ friendsp || friendspOtherWay

isFriendOrEq :: (HasDBPool m, MonadIO m) => UserUUID -> UserUUID -> m Bool
isFriendOrEq user1 user2 = if user1 == user2 then pure True else areFriends user1 user2

checkIsFriendOrEq :: (HasHandler m) => UserUUID -> UserUUID -> m ()
checkIsFriendOrEq user1 user2 =
  unlessM (isFriendOrEq user1 user2) $
    throwError S.err401{S.errBody = "Users are not friends or the same user."}

checkIsFriend :: (HasHandler m) => UserUUID -> UserUUID -> m ()
checkIsFriend user1 user2 =
  unlessM (areFriends user1 user2) $
    throwError S.err401{S.errBody = "Users are not friends."}

checkIsEqUser :: (HasHandler m) => UserUUID -> UserUUID -> m ()
checkIsEqUser user1 user2 =
  unless (user1 == user2) $
    throwError S.err401{S.errBody = "Users are not the same."}

getFeedPosts :: (HasHandler m) => UserUUID -> m [P.Entity DB.Post]
getFeedPosts userId = do
  friendIds <- fmap (.uuid) <$> getFriends userId
  runDB $ P.selectList [PostUser <-. friendIds] [P.Desc PostCreatedAt]

getUserPosts :: (HasHandler m) => UserUUID -> m [P.Entity DB.Post]
getUserPosts requestedUserId = runDB $ P.selectList [PostUser ==. requestedUserId] [P.Desc PostCreatedAt]

getUserApiPosts :: (HasHandler m) => UserUUID -> m [WithUUID ApiPost]
getUserApiPosts requestedUserId = do
  postList <- getUserPosts requestedUserId
  traverse (dbPostToApiPost requestedUserId) postList

redirect303 :: (MonadError S.ServerError m) => Text -> m a
redirect303 url = throwError $ err303{S.errHeaders = [("Location", T.encodeUtf8 url)]}

type family (-->) (l :: [Type]) (a :: Type) where
  '[] --> a = a
  (x ': xs) --> a = x -> (xs --> a)

type Doc (str :: Symbol) (a :: Type) = a

-- userId2 traded a post for postId1 or someone else traded postId1 vor some post of userId2
postWasTradedWith :: (HasHandler m) => PostUUID -> UserUUID -> m Bool
postWasTradedWith postId1 userId2 = runDB do
  tr1p <- P.exists [TradedPostPost1 ==. postId1, TradedPostUser2 ==. userId2]
  tr2p <- P.exists [TradedPostPost2 ==. postId1, TradedPostUser1 ==. userId2]
  pure $ tr1p || tr2p

postTradedForOf :: (HasHandler m) => PostUUID -> UserUUID -> m (Maybe PostUUID)
postTradedForOf postId1 userId2 = runDB do
  postIdM <- fmap (.val.post2) <$> P.getBy (UniqueTradedPostPost1User2 postId1 userId2)
  postIdM' <- fmap (.val.post1) <$> P.getBy (UniqueTradedPostPost2User1 postId1 userId2)
  pure $ postIdM <|> postIdM'

hiddenPostP :: (HasHandler m) => P.Key Post -> Doc "postUser" UserUUID -> m Bool
hiddenPostP postKey postUserId = do
  -- add assertion checking if user is postuser
  hiddenPosts <-
    runDB (P.selectList [PostUser ==. postUserId] [P.Desc PostCreatedAt, P.LimitTo 3])
      <&> filter (\postEnt -> not postEnt.val.deleted)
  let hiddenPostIds = hiddenPosts <&> (.entityKey)
  pure $ postKey `elem` hiddenPostIds

tradesBetween :: (HasHandler m) => UserUUID -> UserUUID -> m [Entity DB.TradedPost]
tradesBetween userId1 userId2 = do
  posts1 <- runDB $ P.selectList [TradedPostUser1 ==. userId1, TradedPostUser2 ==. userId2] []
  posts2 <- runDB $ P.selectList [TradedPostUser1 ==. userId2, TradedPostUser2 ==. userId1] []
  pure $ posts1 <> posts2

getPostIdsTradedWith :: (HasHandler m) => PostUUID -> m [PostUUID]
getPostIdsTradedWith postId = runDB do
  tradedPosts1 <- (fmap (.val.post2)) <$> P.selectList [TradedPostPost1 ==. postId] []
  tradedPosts2 <- (fmap (.val.post1)) <$> P.selectList [TradedPostPost2 ==. postId] []
  pure $ tradedPosts1 <> tradedPosts2

dbPostToApiPost :: (HasHandler m) => UserUUID -> Entity DB.Post -> m (WithUUID ApiPost)
dbPostToApiPost requestingUserId dbPostEnt = do
  let dbPost = dbPostEnt.val
      postUserId = dbPost.user

  ct <- getUTCTime
  apiPost <-
    if postUserId == requestingUserId
      then do
        tradedWithPostIds <- getPostIdsTradedWith dbPostEnt.key
        tradedWithPostEnts <- runDB $ P.selectList [PostId <-. (packKey <$> tradedWithPostIds)] []
        pure $ UnhiddenPost $ UnhiddenPostData dbPost (entityToWithUUID <$> tradedWithPostEnts)
      else do
        -- hide the last three posts

        hiddenP <- hiddenPostP dbPostEnt.entityKey postUserId
        if hiddenP
          then do
            tradedForM <- postTradedForOf dbPostEnt.key requestingUserId
            HiddenPost <$> case tradedForM of
              Just tradedForPostId -> do
                -- FIXME: post could be deleted
                tradedForPost <- getByKeySE @Post tradedForPostId
                pure $ UnlockedHiddenPost $ UnlockedHiddenPostData{post = dbPost, unlockedWithPost = WithUUID tradedForPostId tradedForPost}
              Nothing ->
                pure $
                  let DB.Post{..} = dbPost
                   in LockedHiddenPost LockedHiddenPostData{title, user, createdAt = ct}
          else pure $ UnhiddenPost $ UnhiddenPostData dbPost []
  pure WithUUID{uuid = dbPostEnt.key, value = apiPost}

postIdsToApiPostsForUserWithCheck :: (HasHandler m) => UserUUID -> [PostUUID] -> m [WithUUID ApiPost]
postIdsToApiPostsForUserWithCheck userId postIds = do
  postList <- selectByKey @DB.Post postIds
  for_ postList \post -> checkIsFriendOrEq userId post.val.user
  traverse (dbPostToApiPost userId) postList
