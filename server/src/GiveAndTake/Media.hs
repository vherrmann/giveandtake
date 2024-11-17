module GiveAndTake.Media where

import Control.Exception (throw)
import Control.Monad.Catch
import Data.Text qualified as T
import Database.Persist ((=.))
import Database.Persist qualified as P
import GiveAndTake.DB
import GiveAndTake.Job.Utils (createJob, getByKeyJE)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant qualified as S
import Servant.Multipart qualified as SM
import System.Directory qualified as D
import System.IO (FilePath)
import System.IO.Temp qualified as TMP
import System.Process.Typed qualified as P
import UnliftIO

hasTopTypeOf :: Text -> [Text] -> Bool
hasTopTypeOf mtype toptypes = maybe False (`elem` toptypes) $ headMaybe $ T.splitOn "/" mtype

isImage :: SM.FileData a -> Bool
isImage file = hasTopTypeOf (SM.fdFileCType file) ["image"]

isMedia :: SM.FileData a -> Bool
isMedia file = hasTopTypeOf (SM.fdFileCType file) ["image", "video"]

runProcessWOStdout :: (MonadIO m) => P.ProcessConfig stdin stdout stderr -> m P.ExitCode
runProcessWOStdout conf = do
  P.runProcess . P.setStdout P.nullStream $ conf

-- hacky, but /dev/null doesn't work due to some weird reasons in the system service
-- runProcessWOStdout conf = do
--   (exitC, out) <- P.readProcessStdout . P.setStdout P.byteStringOutput $ conf
--   -- force result
--   evaluate (BL.length out)
--   pure exitC

mediaPath :: UConfig -> MediaId -> FilePath
mediaPath uconfig mediaId = [fmt|{uconfig.mediaDir}/{mediaId}|]

mediaPathOrig :: UConfig -> MediaId -> FilePath
mediaPathOrig uconfig mediaId = mediaPath uconfig mediaId <> ".original"

compressAndConvertImage :: (MonadIO m) => Maybe (Int, Int) -> FilePath -> FilePath -> m P.ExitCode
compressAndConvertImage scaleArg tmpPath newTmpPath =
  -- FIXME: Is magick secure (remote code execution, …)?
{- FOURMOLU_DISABLE -}
  runProcessWOStdout $
    P.proc "magick" $
      [ "convert"
      , "-quality" , "75"
      , "-strip" -- Remove metadata
      ] <> scaleOpt <>
      [ tmpPath
      , newTmpPath
      ]
 where
  scaleOpt = case scaleArg of
    Nothing -> []
    Just (w, h) -> ["-resize", [fmt|{w}x{h}!|]]

{- FOURMOLU_ENABLE -}

compressAndConvertVideo :: (MonadIO m) => FilePath -> FilePath -> m P.ExitCode
compressAndConvertVideo tmpPath newTmpPath =
  -- FIXME: Is ffmpeg secure (remote code execution, …)?
  -- FIXME: timout after certain time
  runProcessWOStdout
{- FOURMOLU_DISABLE -}
    ( P.proc
        "ffmpeg"
        [ "-i" , tmpPath  -- Inputfile
        , "-y" -- Accept all options
        , "-nostdin" -- Don't wait for stdin (blocks all threads)
        , "-v", "warning" -- Don't spam stderr
        -- Replace metadata
        , "-map_metadata", "-1"
        , "-metadata", "title=''"
        , "-metadata", "creation_time=1970-01-01T00:00:00"
        , "-map_chapters", "-1"
        , "-r", "24"  -- Framerate
        , "-vcodec", "libx264" -- Codec
        , "-crf", "23" -- Quality
        , "-preset", "medium" -- Speed/Quality
        , "-vf", "scale='min(1280, iw)':-1, scale=-1:'min(720, ih)'" -- Scale down
        , "-b:a", "128k" -- Set audio bitrate
        , newTmpPath -- result
        ]
    )
{- FOURMOLU_ENABLE -}

type HasMediaJob m = (HasUConfig m, HasDBPool m, MonadIO m, MonadMask m, MonadCatch m, MonadUnliftIO m)

runMediaJob :: (HasMediaJob m) => GATJobMediaUploadData -> m JobResult
runMediaJob (GATJobMediaUploadData{files, userId, reason}) = do
  uconfig <- askM @UConfig
  forConcurrently_ files \file -> do
    let origPath = mediaPathOrig uconfig file.mediaId
    let newTmpPathTemplate = case file.cType of
          MimeImage -> "image.webp"
          MimeVideo -> "video.mp4"
        scaleOpts = case reason of
          MUReasonPost -> Nothing
          MUReasonUserAvatar -> Just (512, 512)
    TMP.withSystemTempFile newTmpPathTemplate \newTmpPath _ -> do
      -- compress file
      exitCode <- case file.cType of
        MimeImage -> compressAndConvertImage scaleOpts origPath newTmpPath
        MimeVideo -> compressAndConvertVideo origPath newTmpPath
      when (exitCode /= P.ExitSuccess) $
        throwIO $
          JobError [fmt|Error while processing file {file.name}.|]

      -- check file size
      fsize <- liftIO $ D.getFileSize newTmpPath
      when (fsize > 20 * 1024 * 1024) $ throwIO $ JobError [fmt|Uploaded file {file.name} is too large.|]
      -- update database
      runDB $ P.update file.mediaId [MediaIsCompressed =. True]
      -- move file
      let comprPath = mediaPath uconfig file.mediaId
      ensureDirOfPath comprPath -- move this to the beginning of the program?
      liftIO $ D.renameFile newTmpPath comprPath
      liftIO $ D.removeFile origPath
  case reason of
    MUReasonPost -> pure ()
    MUReasonUserAvatar -> case files of
      [file] -> do
        draftMedia <- getByKeyJE file.mediaId
        mediaId <- draftToMedia (Entity file.mediaId draftMedia)
        runDB $ P.update userId [#avatar =. Just mediaId]
      _ -> throw $ JobError [fmt|Too many files in avatar upload job|]
  pure $ GATJobResultMediaUpload (files <&> (.mediaId))

insertMedia :: (HasHandler m) => Entity User -> MUReason -> [SM.FileData SM.Tmp] -> m JobId
insertMedia userEnt reason tmpFiles = do
  unless (all isMedia tmpFiles) $
    let notMedia = show @Text . fmap SM.fdFileName . filter (not . isMedia) $ tmpFiles
     in throwError S.err400{S.errBody = [fmt|"Uploaded files {notMedia} are not an image or video."|]}

  uconfig :: UConfig <- askM

  ct <- getUTCTime
  files <- for tmpFiles \file -> do
    mediaId <-
      runDB $
        insertUUID
          Media
            { user = userEnt.key
            , mimeType = file.fdFileCType
            , createdAt = ct
            , isDraft = True
            , isCompressed = False
            }
    topType <-
      if
        | hasTopTypeOf file.fdFileCType ["image"] -> pure MimeImage
        | hasTopTypeOf file.fdFileCType ["video"] -> pure MimeVideo
        -- Should be impossible
        | otherwise -> throwError S.err400{S.errBody = [fmt|"Uploaded file {file.fdFileName} is not an image or video."|]}
    -- the files are in a temporary place, we need to move them to the media directory

    -- move file
    let origPath = mediaPathOrig uconfig mediaId
    ensureDirOfPath origPath
    liftIO $ D.renameFile file.fdPayload origPath

    pure MediaUploadFile{mediaId, name = file.fdFileName, cType = topType}

  -- compress & convert files
  createJob $ GATJobMediaUpload GATJobMediaUploadData{userId = userEnt.key, files, reason}

deleteMedia :: (MonadIO m, HasUConfig m, HasDBPool m, MonadLogger m) => [MediaId] -> m ()
deleteMedia mediaIds = do
  uconf <- askM @UConfig
  runDB $ for_ mediaIds P.delete
  let files = mediaPath uconf <$> mediaIds
  for_ files \file -> do
    existsP <- liftIO $ D.doesFileExist file
    if existsP
      then liftIO $ D.removeFile file
      else logInfo [fmt|"File {file} does not exist when trying to delete it."|]

draftToMedia :: (HasUConfig m, HasDBPool m, MonadIO m) => Entity Media -> m MediaId
draftToMedia mediaEnt = do
  uconf :: UConfig <- askM
  -- Check if draft, if the medium is not a draft, it is already used by another post
  if mediaEnt.val.isDraft
    then mediaEnt.key <$ runDB (P.update mediaEnt.key [MediaIsDraft =. False])
    else do
      -- copy files (each post should have unique media files, so that we can easily delete them)
      newMediaId <-
        runDB $
          insertUUID $
            Media
              { user = mediaEnt.val.user -- should be equal to entityKey user
              , mimeType = mediaEnt.val.mimeType
              , isDraft = False
              , isCompressed = mediaEnt.val.isCompressed
              , createdAt = mediaEnt.val.createdAt
              }
      -- Move file
      let mediaPath = [fmt|{uconf.mediaDir}/{mediaEnt.key}|]
      let newMediaPath = [fmt|{uconf.mediaDir}/{newMediaId}|]
      liftIO $ D.copyFile mediaPath newMediaPath
      pure newMediaId
