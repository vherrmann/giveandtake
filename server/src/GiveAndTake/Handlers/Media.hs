module GiveAndTake.Handlers.Media where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID qualified as U
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils (checkIsFriendOrEq)
import GiveAndTake.Job (createJob)
import GiveAndTake.Media
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Network.Wai.Application.Static qualified as W
import Servant (ServerError (..), err404, type (:<|>) (..))
import Servant qualified as S
import Servant.Multipart qualified as SM
import System.Directory qualified as D
import WaiAppStatic.Types qualified as W

mediaHandler :: Entity User -> RServer m MediaApi
mediaHandler userEnt = uploadMediaH userEnt :<|> getMediaH userEnt

uploadMediaH :: (HasHandler m) => Entity User -> UploadMedia -> m JobUUID
uploadMediaH userEnt umedia = do
  -- check number of files
  when (null umedia.files) $ throwError S.err400{errBody = "Uploaded no file."}
  when (length umedia.files > 20) $ throwError S.err400{errBody = "Uploaded too many files."}
  -- check file formats
  unless (all isMedia umedia.files) $
    let notMedia = show @Text . fmap SM.fdFileName . filter (not . isMedia) $ umedia.files
     in throwError S.err400{errBody = [fmt|"Uploaded files {notMedia} are not an image or video."|]}

  uconfig :: UConfig <- askM

  ct <- getUTCTime
  files <- for umedia.files \file -> do
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

  createJob $ GATJobMediaUpload GATJobMediaUploadData{userId = userEnt.key, files}

-- compress & convert files

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
