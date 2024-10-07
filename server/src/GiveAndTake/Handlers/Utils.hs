{-# LANGUAGE AllowAmbiguousTypes #-}

module GiveAndTake.Handlers.Utils where

import Control.Monad.Error.Class (MonadError (..))
import Data.Text.Encoding qualified as T
import Database.Persist ((<-.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types
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
checkIsFriendOrEq user1 user2 = unlessM (isFriendOrEq user1 user2) $ throwError S.err401

checkIsEqUser :: (HasHandler m) => UserUUID -> UserUUID -> m ()
checkIsEqUser user1 user2 = unless (user1 == user2) $ throwError S.err401

getFeedPosts :: (HasHandler m) => UserUUID -> m [P.Entity Post]
getFeedPosts userId = do
  friendIds <- fmap (.uuid) <$> getFriends userId
  runDB $ P.selectList [PostUser <-. friendIds] [P.Desc PostCreatedAt]

redirect303 :: (MonadError S.ServerError m) => Text -> m a
redirect303 url = throwError $ err303{S.errHeaders = [("Location", T.encodeUtf8 url)]}

type family (-->) (l :: [Type]) (a :: Type) where
  '[] --> a = a
  (x ': xs) --> a = x -> (xs --> a)
