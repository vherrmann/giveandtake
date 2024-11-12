module GiveAndTake.Handlers.Posts where

import Data.List (zip, zipWith)
import Data.String (String)
import Data.Text qualified as T
import Database.Persist ((=.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.DB.Types qualified as DB
import GiveAndTake.Handlers.Utils
import GiveAndTake.Media
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant (type (:<|>) (..))
import Servant qualified as S
import System.Directory qualified as D

postsHandler :: Entity User -> RServer m PostsApi
postsHandler userEnt =
  getPostH userEnt
    :<|> deletePostH userEnt
    :<|> createPostH userEnt
    :<|> getTradeablePostsH userEnt
    :<|> postTradePostsH userEnt
    :<|> getPostsFeedH userEnt
    :<|> postsLikeHandler userEnt

getPostsFeedH :: (HasHandler m) => Entity User -> m [WithKey Post ApiPost]
getPostsFeedH userEntity = traverse (dbPostToApiPost userEntity.key) =<< getFeedPosts userEntity.key

getPostH :: (HasHandler m) => Entity User -> PostId -> m ApiPost
getPostH userEnt postId = do
  postEnt <- getByKeySEEnt @DB.Post postId
  checkIsFriendOrEq userEnt.key postEnt.val.user
  dbPostToApiPost userEnt.key postEnt <&> (.value)

-- selectPosts :: Handler [Post]
-- selectPosts = do
--   postList <- runDB $ selectList [] []
--   pure $ map (\(Entity _ u) -> u) postList

createPostH :: (HasHandler m) => Entity User -> NewPost -> m PostId
createPostH user newpost = do
  media <- traverse getByKeySE newpost.media

  when (T.length newpost.title > 24) $
    throwError S.err409{S.errBody = "Title cannot be longer than 24 characters."}

  unless (all ((.isCompressed)) media) $
    throwError S.err400{S.errBody = "Some media aren't completely compressed yet."}

  mediaIds <- for (zipWith P.Entity newpost.media media) draftToMedia

  ct <- getUTCTime
  runDB $
    insertUUID
      ( Post
          { title = newpost.title
          , media = mediaIds
          , body = newpost.body
          , user = user.key
          , deleted = False
          , createdAt = ct
          }
      )

deletePostH :: (HasHandler m) => Entity User -> PostId -> m ()
deletePostH userEnt postId = do
  post <- getByKeySE @DB.Post postId
  checkIsEqUser userEnt.key post.user
  runDB $
    P.update
      postId
      [ DB.PostTitle =. ""
      , DB.PostMedia =. []
      , DB.PostBody =. ""
      , DB.PostDeleted =. True
      ]
  -- Delete media
  deleteMedia post.media

getTradeablePostsH :: (HasHandler m) => Entity User -> UserId -> m [WithKey' DB.Post]
getTradeablePostsH userEnt postUser = do
  checkIsFriendOrEq userEnt.key postUser
  postList <- getUserPosts userEnt.key
  apiPostList <- zip postList <$> traverse (dbPostToApiPost postUser) postList
  let tradeablePostEnts =
        [ postEnt
        | (postEnt, WithKey _id (HiddenPost (LockedHiddenPost _))) <- apiPostList
        ]
  pure $ entityToWithKey <$> tradeablePostEnts

postTradePostsH :: (HasHandler m) => Entity User -> PostId -> PostId -> m ()
postTradePostsH userEnt post1Id post2Id = do
  post1 <- getByKeySE @DB.Post post1Id
  post2 <- getByKeySE @DB.Post post2Id
  checkIsEqUser userEnt.key post1.user
  checkIsFriend userEnt.key post2.user
  -- check if post2 was already traded for some post of UserEnt
  whenM (postWasTradedWith post2Id userEnt.key) $ throwError S.err409{S.errBody = "Post was already traded with some post of yours."}
  unlessM (hiddenPostP post2Id post2.user) $ throwError S.err409{S.errBody = "Post is not hidden."}

  ct <- getUTCTime
  runDB $
    P.insert_ $
      TradedPost
        { post1 = post1Id
        , user1 = post1.user
        , post2 = post2Id
        , user2 = post2.user
        , createdAt = ct
        }

postsLikeHandler :: Entity User -> RServer m PostsLikeApi
postsLikeHandler userEnt =
  likePostH userEnt
    :<|> unlikePostH userEnt

unlikePostH :: (HasHandler m) => Entity User -> PostId -> m ()
unlikePostH userEnt postId = do
  post <- getByKeySE @DB.Post postId
  -- NOTE: users should not be equal
  checkIsFriend userEnt.key post.user
  -- FIXME: throw error if heart not found?
  runDB $ P.deleteBy $ UniquePostHeartPostUser postId userEnt.key

likePostH :: (HasHandler m) => Entity User -> PostId -> m ()
likePostH userEnt postId = do
  post <- getByKeySE @DB.Post postId
  -- NOTE: users should not be equal
  checkIsFriend userEnt.key post.user
  ct <- getUTCTime
  void $ runDB $ P.insert PostHeart{user = userEnt.key, post = postId, createdAt = ct}
