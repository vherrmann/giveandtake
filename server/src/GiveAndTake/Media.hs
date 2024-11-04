module GiveAndTake.Media where

import Control.Monad.Catch
import Data.Text qualified as T
import Data.UUID qualified as U
import Database.Persist ((=.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant.Multipart qualified as SM
import System.Directory qualified as D
import System.IO (FilePath)
import System.IO.Temp qualified as TMP
import System.Process.Typed qualified as P
import UnliftIO

hasTopTypeOf :: Text -> [Text] -> Bool
hasTopTypeOf mtype toptypes = maybe False (`elem` toptypes) $ headMaybe $ T.splitOn "/" mtype

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

mediaPath :: UConfig -> MediaUUID -> FilePath
mediaPath uconfig uuid = [fmt|{uconfig.mediaDir}/{U.toText uuid}|]

mediaPathOrig :: UConfig -> MediaUUID -> FilePath
mediaPathOrig uconfig uuid = mediaPath uconfig uuid <> ".original"

compressAndConvertImage :: (MonadIO m) => FilePath -> FilePath -> m P.ExitCode
compressAndConvertImage tmpPath newTmpPath =
  -- FIXME: Is magick secure (remote code execution, …)?
  runProcessWOStdout
{- FOURMOLU_DISABLE -}
    ( P.proc
        "magick"
        [ "convert"
        , "-quality", "75"
        , "-strip" -- Remove metadata
        , tmpPath
        , newTmpPath
        ]
    )
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

type HasMediaJob m = (HasUConfig m, HasDBPool m, MonadIO m, MonadMask m, MonadCatch m)

runMediaJob :: (HasMediaJob m) => GATJobMediaUploadData -> m JobResult
runMediaJob (GATJobMediaUploadData{files}) = do
  uconfig <- askM @UConfig
  for_ files \file -> do
    let origPath = mediaPathOrig uconfig file.mediaId
    let newTmpPathTemplate = case file.cType of
          MimeImage -> "image.webp"
          MimeVideo -> "video.mp4"
    TMP.withSystemTempFile newTmpPathTemplate \newTmpPath _ -> do
      -- compress file
      exitCode <- case file.cType of
        MimeImage -> compressAndConvertImage origPath newTmpPath
        MimeVideo -> compressAndConvertVideo origPath newTmpPath
      when (exitCode /= P.ExitSuccess) $
        throwIO $
          JobError [fmt|"Error while processing file {file.name}."|]

      -- check file size
      fsize <- liftIO $ D.getFileSize newTmpPath
      when (fsize > 20 * 1024 * 1024) $ throwIO $ JobError [fmt|"Uploaded file {file.name} is too large."|]
      -- update database
      runDB $ P.update (packKey @Media file.mediaId) [MediaIsCompressed =. True]
      -- move file
      let comprPath = mediaPath uconfig file.mediaId
      ensureDirOfPath comprPath -- move this to the beginning of the program?
      liftIO $ D.renameFile newTmpPath comprPath
      liftIO $ D.removeFile origPath
  pure $ GATJobResultMediaUpload (files <&> (.mediaId))
