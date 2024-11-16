module GiveAndTake.Handlers.Media where

import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID qualified as U
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils (checkIsFriendOrEq)
import GiveAndTake.Job.Utils (createJob)
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

uploadMediaH :: (HasHandler m) => Entity User -> UploadMedia -> m JobId
uploadMediaH userEnt umedia = do
  -- check number of files
  when (null umedia.files) $ throwError S.err400{errBody = "Uploaded no file."}
  when (length umedia.files > 20) $ throwError S.err400{errBody = "Uploaded too many files."}
  -- check file formats
  insertMedia userEnt MUReasonPost umedia.files

-- FIXME: Use octetstream instead of abused static file server (or implement proper file server as wai application)
-- FIXME: check if user has access to post (hiddenpost) or profilepicture
getMediaH :: Entity User -> MediaId -> RServer m S.RawM
getMediaH userEnt mediaId req onResponse = do
  uconfig :: UConfig <- askM
  media <- getByKeySE @Media mediaId
  checkIsFriendOrEq userEnt.key media.user

  let mimeType = T.encodeUtf8 media.mimeType
  -- Try to convert the mediaId to a path piece for a static file server (dots and slashes are not allowed)
  uuidPiece <-
    maybeToMErr (err404{errBody = "Impossible! Also this incident will be reported to the webmaster."}) $
      W.toPiece (U.toText $ coerce mediaId)
  -- Create config
  let defConfig = W.defaultFileServerSettings (T.unpack uconfig.mediaDir)
      origLookupFile = W.ssLookupFile defConfig
      -- Insert the mediaId as the constant piece to the lookup function
      authorizedLookupFile = origLookupFile . const [uuidPiece]
      config =
        W.StaticSettings
          { W.ssLookupFile = authorizedLookupFile
          , W.ssGetMimeType = const (pure mimeType)
          , -- Agressive Caching
            W.ssMaxAge = W.MaxAgeSeconds (60 * 60 * 24 * 365)
          , W.ssUseHash = False
          , -- No extra stuff
            W.ssListing = Nothing
          , W.ssIndices = []
          , W.ssMkRedirect = const id
          , W.ssRedirectToIndex = False
          , W.ssAddTrailingSlash = False
          , W.ss404Handler = Nothing
          }
  liftIO $ S.unTagged (S.serveDirectoryWith config) req onResponse
