module GiveAndTake.Handlers.Posts where

import Data.List (zip)
import Data.String (String)
import Data.Text qualified as T
import Database.Persist ((=.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.DB.Types qualified as DB
import GiveAndTake.Handlers.Utils
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
  uconf :: UConfig <- askM
  media <- traverse getByKeySE newpost.media

  when (T.length newpost.title > 24) $
    throwError S.err409{S.errBody = "Title cannot be longer than 24 characters."}

  unless (all ((.isCompressed)) media) $
    throwError S.err400{S.errBody = "Some media aren't completely compressed yet."}

  mediaIds <- for (zip media newpost.media) \(medium, mediaId) -> do
    -- Check if draft, if the medium is not a draft, it is already used by another post
    if medium.isDraft
      then runDB $ P.update mediaId [MediaIsDraft =. False]
      else do
        -- copy files (each post should have unique media files, so that we can easily delete them)
        newMediaId <-
          runDB $
            insertUUID $
              Media
                { user = medium.user -- should be equal to entityKey user
                , mimeType = medium.mimeType
                , isDraft = False
                , isCompressed = False
                , createdAt = medium.createdAt
                }
        -- Move file
        let mediaPath = [fmt|{uconf.mediaDir}/{mediaId}|]
        let newMediaPath = [fmt|{uconf.mediaDir}/{newMediaId}|]
        liftIO $ D.copyFile mediaPath newMediaPath
    pure mediaId

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
  uconf :: UConfig <- askM
  for_ post.media \mediaId -> do
    mmedia <- runDB (P.get mediaId)
    case mmedia of
      Nothing -> pure () -- FIXME
      Just _ -> do
        runDB $ P.delete mediaId
        let mediaPath = [fmt|{uconf.mediaDir}/{mediaId}|]
        existsP <- liftIO $ D.doesFileExist mediaPath
        if existsP
          then liftIO $ D.removeFile mediaPath
          else -- FIXME: proper logging system
            putStrLn @String [fmt|"File {mediaPath} does not exist when trying to delete it."|]
        runDB $ P.delete mediaId

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
