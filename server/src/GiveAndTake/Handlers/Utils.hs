{-# LANGUAGE AllowAmbiguousTypes #-}

module GiveAndTake.Handlers.Utils where

import Control.Monad.Error.Class (MonadError (..))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding qualified as T
import Database.Persist ((<-.), (==.))
import Database.Persist qualified as P
import GHC.Base (Symbol)
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.DB.Types qualified as DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils (Doc, getUTCTime, validateTokenEither)
import Servant (err303)
import Servant qualified as S

getFriends :: (HasDBPool m, MonadIO m, MonadLogger m) => UserId -> m [WithKey User UserPublic]
getFriends userId = do
  listFstIds <- runDB $ fmap (.val.user2) <$> P.selectList [FriendsUser1 ==. userId] []
  listSndIds <- runDB $ fmap (.val.user1) <$> P.selectList [FriendsUser2 ==. userId] []

  friends <- selectByKey @User (listFstIds <> listSndIds)
  pure $ friends <&> \user -> WithKey user.key (userToPublic user.val)

areFriends :: (HasDBPool m, MonadIO m) => UserId -> UserId -> m Bool
areFriends user1 user2 = do
  friendsp <- runDB $ P.exists [FriendsUser1 ==. user1, FriendsUser2 ==. user2]
  friendspOtherWay <- runDB $ P.exists [FriendsUser1 ==. user2, FriendsUser2 ==. user1]

  pure $ friendsp || friendspOtherWay

isFriendOrEq :: (HasDBPool m, MonadIO m) => UserId -> UserId -> m Bool
isFriendOrEq user1 user2 = if user1 == user2 then pure True else areFriends user1 user2

isGroupMember :: (MonadIO m, HasDBPool m) => UserId -> GroupId -> m Bool
isGroupMember userId groupId = runDB $ P.existsBy (UniqueGroupMember groupId userId)

isGroupOwner :: (MonadIO m, HasDBPool m) => UserId -> GroupId -> m Bool
isGroupOwner userId groupId = do
  memberp <- isGroupMember userId groupId
  groupM <- runDB (P.get groupId)
  pure $ case groupM of
    Nothing -> False
    Just group -> memberp && (userId == group.owner)

checkIsFriendOrEq :: (HasHandler m) => UserId -> UserId -> m ()
checkIsFriendOrEq user1 user2 =
  unlessM (isFriendOrEq user1 user2) $
    throwError S.err401{S.errBody = "Users are not friends or the same user."}

checkIsGroupMember :: (HasHandler m) => UserId -> GroupId -> m ()
checkIsGroupMember userId groupId =
  unlessM (isGroupMember userId groupId) $
    throwError S.err401{S.errBody = "User is not a member of the group."}

isGroupHigher :: (MonadIO m, HasDBPool m) => UserId -> UserId -> GroupId -> m Bool
isGroupHigher user1Id user2Id groupId =
  if user1Id == user2Id
    then pure False
    else runDB do
      groupMemb1M <- P.getBy (UniqueGroupMember groupId user1Id)
      groupMemb2M <- P.getBy (UniqueGroupMember groupId user2Id)
      groupM <- P.get groupId
      pure $ case (groupMemb1M, groupMemb2M, groupM) of
        (Just groupMemb1, Just groupMemb2, Just group) ->
          (groupMemb2.val.user /= group.owner)
            && ( (groupMemb1.val.user == group.owner)
                  || (groupMemb1.val.role > groupMemb2.val.role)
               )
        _ -> False

checkIsGroupHigher :: (HasHandler m) => UserId -> UserId -> GroupId -> m ()
checkIsGroupHigher user1Id user2Id groupId =
  unlessM (isGroupHigher user1Id user2Id groupId) $
    throwError S.err401{S.errBody = "User is not higher in the group hierarchy."}

isGroupAdmin :: (MonadIO m, HasDBPool m) => UserId -> GroupId -> m Bool
isGroupAdmin userId groupId = runDB do
  groupMembM <- P.getBy (UniqueGroupMember groupId userId)
  groupM <- P.get groupId
  pure $ case (groupMembM, groupM) of
    (Just groupMemb, Just group) -> groupMemb.val.role == GroupRoleAdmin || group.owner == userId
    _ -> False

checkIsGroupAdmin :: (HasHandler m) => UserId -> GroupId -> m ()
checkIsGroupAdmin userId groupId =
  unlessM (isGroupAdmin userId groupId) $
    throwError S.err401{S.errBody = "User is not an admin of the group."}

checkIsGroupOwner :: (HasHandler m) => UserId -> GroupId -> m ()
checkIsGroupOwner userId groupId =
  unlessM (isGroupOwner userId groupId) $
    throwError S.err401{S.errBody = "User is not the owner of the group."}

checkIsFriend :: (HasHandler m) => UserId -> UserId -> m ()
checkIsFriend user1 user2 =
  unlessM (areFriends user1 user2) $
    throwError S.err401{S.errBody = "Users are not friends."}

checkIsEqUser :: (HasHandler m) => UserId -> UserId -> m ()
checkIsEqUser user1 user2 =
  unless (user1 == user2) $
    throwError S.err401{S.errBody = "Users are not the same."}

getFeedPosts :: (HasHandler m) => UserId -> m [P.Entity DB.Post]
getFeedPosts userId = do
  friendIds <- fmap (.key) <$> getFriends userId
  runDB $ P.selectList [PostUser <-. friendIds] [P.Desc PostCreatedAt]

getUserPosts :: (HasHandler m) => UserId -> m [P.Entity DB.Post]
getUserPosts requestedUserId = runDB $ P.selectList [PostUser ==. requestedUserId] [P.Desc PostCreatedAt]

getUserApiPosts :: (HasHandler m) => Entity User -> UserId -> m [WithKey Post ApiPost]
getUserApiPosts userEnt requestedUserId = do
  postList <- getUserPosts requestedUserId
  traverse (dbPostToApiPost userEnt.key) postList

redirect303 :: (MonadError S.ServerError m) => Text -> m a
redirect303 url = throwError $ err303{S.errHeaders = [("Location", T.encodeUtf8 url)]}

type family (-->) (l :: [Type]) (a :: Type) where
  '[] --> a = a
  (x ': xs) --> a = x -> (xs --> a)

-- userId2 traded a post for postId1 or someone else traded postId1 vor some post of userId2
postWasTradedWith :: (HasHandler m) => PostId -> UserId -> m Bool
postWasTradedWith postId1 userId2 = runDB do
  tr1p <- P.exists [TradedPostPost1 ==. postId1, TradedPostUser2 ==. userId2]
  tr2p <- P.exists [TradedPostPost2 ==. postId1, TradedPostUser1 ==. userId2]
  pure $ tr1p || tr2p

postTradedForOf :: (HasHandler m) => PostId -> UserId -> m (Maybe PostId)
postTradedForOf postId1 userId2 = runDB do
  postIdM <- fmap (.val.post2) <$> P.getBy (UniqueTradedPostPost1User2 postId1 userId2)
  postIdM' <- fmap (.val.post1) <$> P.getBy (UniqueTradedPostPost2User1 postId1 userId2)
  pure $ postIdM <|> postIdM'

hiddenPostP :: (HasHandler m) => P.Key Post -> Doc "postUser" UserId -> m Bool
hiddenPostP postKey postUserId = do
  -- add assertion checking if user is postuser
  hiddenPosts <-
    runDB (P.selectList [PostUser ==. postUserId] [P.Desc PostCreatedAt, P.LimitTo 3])
      <&> filter (\postEnt -> not postEnt.val.deleted)
  let hiddenPostIds = hiddenPosts <&> (.entityKey)
  pure $ postKey `elem` hiddenPostIds

tradesBetween :: (HasHandler m) => UserId -> UserId -> m [Entity DB.TradedPost]
tradesBetween userId1 userId2 = do
  posts1 <- runDB $ P.selectList [TradedPostUser1 ==. userId1, TradedPostUser2 ==. userId2] []
  posts2 <- runDB $ P.selectList [TradedPostUser1 ==. userId2, TradedPostUser2 ==. userId1] []
  pure $ posts1 <> posts2

getPostIdsTradedWith :: (HasHandler m) => PostId -> m [PostId]
getPostIdsTradedWith postId = runDB do
  tradedPosts1 <- (fmap (.val.post2)) <$> P.selectList [TradedPostPost1 ==. postId] []
  tradedPosts2 <- (fmap (.val.post1)) <$> P.selectList [TradedPostPost2 ==. postId] []
  pure $ tradedPosts1 <> tradedPosts2

dbPostToApiPost :: (HasHandler m) => UserId -> Entity DB.Post -> m (WithKey Post ApiPost)
dbPostToApiPost requestingUserId dbPostEnt = do
  let dbPost = dbPostEnt.val
      postUserId = dbPost.user
      getLiked = runDB $ P.existsBy (UniquePostHeartPostUser dbPostEnt.key requestingUserId)

  ct <- getUTCTime
  apiPost <-
    if
      | dbPost.deleted -> pure $ DeletedPost DeletedPostData{user = dbPost.user, createdAt = ct}
      | postUserId == requestingUserId -> do
          tradedWithPostIds <- getPostIdsTradedWith dbPostEnt.key
          tradedWithPostEnts <- runDB $ P.selectList [PostId <-. tradedWithPostIds] []
          liked <- getLiked
          pure $
            UnhiddenPost
              UnhiddenPostData
                { post = ViewablePostData{post = dbPost, liked}
                , usedToUnlock = entityToWithKey <$> tradedWithPostEnts
                }
      | otherwise -> do
          -- hide the last three posts
          hiddenP <- hiddenPostP dbPostEnt.entityKey postUserId
          if hiddenP
            then do
              tradedForM <- postTradedForOf dbPostEnt.key requestingUserId
              HiddenPost <$> case tradedForM of
                Just tradedForPostId -> do
                  -- FIXME: post could be deleted
                  tradedForPost <- getByKeySE @Post tradedForPostId
                  liked <- getLiked
                  pure $
                    UnlockedHiddenPost
                      UnlockedHiddenPostData
                        { post = ViewablePostData{post = dbPost, liked}
                        , unlockedWithPost = WithKey tradedForPostId tradedForPost
                        }
                Nothing ->
                  pure $
                    let DB.Post{..} = dbPost
                     in LockedHiddenPost LockedHiddenPostData{title, user, createdAt = ct}
            else do
              liked <- getLiked
              pure $ UnhiddenPost UnhiddenPostData{post = ViewablePostData{post = dbPost, liked}, usedToUnlock = []}
  pure WithKey{key = dbPostEnt.key, value = apiPost}

postIdsToApiPostsForUserWithCheck :: (HasHandler m) => UserId -> [PostId] -> m [WithKey Post ApiPost]
postIdsToApiPostsForUserWithCheck userId postIds = do
  postList <- selectByKey postIds
  for_ postList \post -> checkIsFriendOrEq userId post.val.user
  traverse (dbPostToApiPost userId) postList

withValidateTokenSE :: (HasHandler m) => Text -> Text -> m a -> m a
withValidateTokenSE token hash m = case validateTokenEither token hash of
  Left errStr -> throwError S.err500{S.errBody = "Server error while checking hash: " <> BL.pack errStr}
  Right False -> throwError S.err401{S.errBody = "Wrong password."}
  Right True -> m
