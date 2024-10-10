module GiveAndTake.Handlers.Posts where

import Data.List (zip)
import Data.String (String)
import Data.UUID (UUID)
import Data.UUID qualified as U
import Database.Persist ((=.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
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
    :<|> getPostsFeedH userEnt

getPostsFeedH :: Entity User -> RHandler m [WithUUID Post]
getPostsFeedH userEntity = fmap entityToWithUUID <$> getFeedPosts (entityUKey userEntity)

getPostH :: (HasHandler m) => Entity User -> PostUUID -> m Post
getPostH userEnt postId = do
  post <- getByKeySE @Post postId
  checkIsFriendOrEq userEnt.key post.user
  pure post

-- selectPosts :: Handler [Post]
-- selectPosts = do
--   postList <- runDB $ selectList [] []
--   pure $ map (\(Entity _ u) -> u) postList

createPostH :: (HasHandler m) => Entity User -> NewPost -> m UUID
createPostH user newpost = do
  uconf :: UConfig <- askM
  media <- traverse (getByKeySE @Media) newpost.media
  unless (all ((.isCompressed)) media) $ throwError S.err400{S.errBody = "Some media aren't completely compressed yet."}

  mediaIds <- for (zip media newpost.media) \(medium, mediaId) -> do
    -- Check if draft, if the medium is not a draft, it is already used by another post
    if medium.isDraft
      then runDB $ P.update (packKey @Media mediaId) [MediaIsDraft =. False]
      else do
        -- copy files (each post should have unique media files, so that we can easily delete them)
        newUuid <-
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
        let mediaPath = [fmt|{uconf.mediaDir}/{U.toText mediaId}|]
        let newMediaPath = [fmt|{uconf.mediaDir}/{U.toText newUuid}|]
        liftIO $ D.copyFile mediaPath newMediaPath
    pure mediaId

  ct <- getUTCTime
  runDB $
    insertUUID
      ( Post
          { title = newpost.title
          , media = mediaIds
          , body = newpost.body
          , user = entityUKey user
          , createdAt = ct
          }
      )

deletePostH :: (HasHandler m) => Entity User -> PostUUID -> m ()
deletePostH userEnt postId = do
  post <- getByKeySE @Post postId
  checkIsEqUser userEnt.key post.user
  runDB $ P.delete $ packKey @Post postId
  -- Delete media
  uconf :: UConfig <- askM
  for_ post.media \mediaUuid -> do
    mmedia <- runDB (P.get $ packKey @Media mediaUuid)
    case mmedia of
      Nothing -> pure () -- FIXME
      Just _ -> do
        runDB $ P.delete $ packKey @Media mediaUuid
        let mediaPath = [fmt|{uconf.mediaDir}/{U.toText mediaUuid}|]
        existsP <- liftIO $ D.doesFileExist mediaPath
        if existsP
          then liftIO $ D.removeFile mediaPath
          else -- FIXME: proper logging system
            putStrLn @String [fmt|"File {mediaPath} does not exist when trying to delete it."|]
        runDB $ P.delete $ MediaKey mediaUuid
