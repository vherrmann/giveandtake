module GiveAndTake.Handlers.Media where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID qualified as U
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils (checkIsFriendOrEq)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Network.Wai.Application.Static qualified as W
import Servant (ServerError (..), err404, type (:<|>) (..))
import Servant qualified as S
import Servant.Multipart qualified as SM
import System.Directory qualified as D
import System.IO (FilePath)
import System.IO.Temp qualified as TMP
import System.Process.Typed qualified as P
import WaiAppStatic.Types qualified as W

mediaHandler :: Entity User -> RServer m MediaApi
mediaHandler userEnt = uploadMediaH userEnt :<|> getMediaH userEnt

hasTopTypeOf :: Text -> [Text] -> Bool
hasTopTypeOf mtype toptypes = maybe False (`elem` toptypes) $ headMaybe $ T.splitOn "/" mtype

isMedia :: SM.FileData a -> Bool
isMedia file = hasTopTypeOf (SM.fdFileCType file) ["image", "video"]

data AllowedMediaTopTypes = MimeImage | MimeVideo

runProcessWOStdout :: (MonadIO m) => P.ProcessConfig stdin stdout stderr -> m P.ExitCode
runProcessWOStdout = P.runProcess . P.setStdout P.nullStream

compressAndConvertImage :: (HasHandler m) => FilePath -> FilePath -> m P.ExitCode
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

compressAndConvertVideo :: (HasHandler m) => FilePath -> FilePath -> m P.ExitCode
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

uploadMediaH :: (HasHandler m) => Entity User -> UploadMedia -> m UploadMediaResponse
uploadMediaH userEnt umedia = do
  uconf :: UConfig <- askM
  -- check number of files
  when (null (files umedia)) $ throwError S.err400{errBody = "Uploaded no file."}
  when (length (files umedia) > 20) $ throwError S.err400{errBody = "Uploaded too many files."}
  -- check file formats
  unless (all isMedia (files umedia)) $
    let notMedia = show @Text . fmap SM.fdFileName . filter (not . isMedia) $ files umedia
     in throwError S.err400{errBody = [fmt|"Uploaded files {notMedia} are not an image or video."|]}
  -- compress & convert files
  mediaIds <- for umedia.files \file -> do
    let tmpPath = file.fdPayload
    topType <-
      if
        | hasTopTypeOf file.fdFileCType ["image"] -> pure MimeImage
        | hasTopTypeOf file.fdFileCType ["video"] -> pure MimeVideo
        | otherwise -> throwError S.err400{errBody = [fmt|"Uploaded file {file.fdFileName} is not an image or video."|]}
    let newTmpPathTemplate = case topType of
          MimeImage -> "image.webp"
          MimeVideo -> "video.mp4"
    TMP.withSystemTempFile newTmpPathTemplate \newTmpPath _ -> do
      -- compress file
      exitCode <- case topType of
        MimeImage -> compressAndConvertImage tmpPath newTmpPath
        MimeVideo -> compressAndConvertVideo tmpPath newTmpPath
      when (exitCode /= P.ExitSuccess) $
        throwError S.err500{errBody = [fmt|"Error while processing file {file.fdFileName}."|]}

      -- check file size
      fsize <- liftIO $ D.getFileSize newTmpPath
      when (fsize > 20 * 1024 * 1024) $ throwError S.err400{errBody = [fmt|"Uploaded file {file.fdFileName} is too large."|]}
      -- update database
      ctime <- getUTCTime
      uuid <-
        runDB $
          insertUUID
            Media
              { user = userEnt.key
              , mimeType = file.fdFileCType
              , createdAt = ctime
              , isDraft = True
              }
      let mediaPath :: FilePath = [fmt|{uconf.mediaDir}/{U.toText uuid}|]
      -- move file
      ensureDirOfPath mediaPath -- move this to the beginning of the program?
      liftIO $ D.renameFile newTmpPath mediaPath
      pure uuid
  pure UploadMediaResponse{..}

-- FIXME: Use octetstream instead of abused static file server (or implement proper file server as wai application)
getMediaH :: Entity User -> MediaUUID -> RServer m S.RawM
getMediaH userEnt uuid req onResponse = do
  uconfig :: UConfig <- askM
  media <- getByKeySE @Media uuid
  checkIsFriendOrEq userEnt.key media.user

  let mimeType = T.encodeUtf8 media.mimeType
  -- Try to convert the uuid to a path piece for a static file server (dots and slashes are not allowed)
  uuidPiece <-
    maybeToMErr (err404{errBody = "Impossible! Also this incident will be reported to the webmaster."}) $
      W.toPiece (U.toText uuid)
  -- Create config
  let defConfig = W.defaultFileServerSettings (T.unpack uconfig.mediaDir)
      origLookupFile = W.ssLookupFile defConfig
      -- Insert the UUID as the constant piece to the lookup function
      authorizedLookupFile = origLookupFile . const [uuidPiece]
      config = defConfig{W.ssLookupFile = authorizedLookupFile, W.ssGetMimeType = const (pure mimeType)}
  liftIO $ S.unTagged (S.serveDirectoryWith config) req onResponse
