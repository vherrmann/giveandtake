{-# OPTIONS_GHC -Wno-orphans #-}

module GiveAndTake.Utils where

import Crypto.KDF.BCrypt (hashPassword, validatePassword, validatePasswordEither)
import Crypto.Random (getRandomBytes)
import Data.ByteString.Base64.URL (encodeBase64)
import Data.Base64.Types (extractBase64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.System
import Data.UUID (UUID)
import Data.UUID qualified as U
import Data.UUID.V4 qualified as U
import GiveAndTake.Prelude

import Data.Coerce
import Database.Persist qualified as P
import GHC.Base (Symbol)
import GiveAndTake.Types
import System.Directory qualified as D
import System.FilePath qualified as F

getUTCTime :: (MonadIO m) => m UTCTime
getUTCTime = liftIO getCurrentTime

-- TODO: Increase hash strength from time to time
hashToken :: (MonadIO m) => Text -> m Text
hashToken tok = liftIO (T.decodeUtf8 <$> hashPassword strength (T.encodeUtf8 tok))
 where
  strength = 12

validateToken :: Text -> Text -> Bool
validateToken tok hash = validatePassword (T.encodeUtf8 tok) (T.encodeUtf8 hash)

validateTokenEither :: Text -> Text -> Either [Char] Bool
validateTokenEither tok hash = validatePasswordEither (T.encodeUtf8 tok) (T.encodeUtf8 hash)

-- FIXME: use jwk
randomUrlToken :: (MonadIO m) => m Text
randomUrlToken = do
  randomBytes <- liftIO $ getRandomBytes 50
  pure $ extractBase64 . encodeBase64 $ randomBytes

randomUUID :: (MonadIO m) => m UUID
randomUUID = liftIO U.nextRandom

ensureDirOfPath :: (MonadIO m) => F.FilePath -> m ()
ensureDirOfPath = liftIO . D.createDirectoryIfMissing True . F.takeDirectory

authUrl :: UConfig -> [Text] -> Text
-- FIXME: replace http by https in production
authUrl uconfig path = [fmt|http://{uconfig.baseUrl}/{T.intercalate "/" path}|]

docsUrl :: UConfig -> [Text] -> Text
docsUrl uconfig path = [fmt|http://{uconfig.docsBaseUrl}/{T.intercalate "/" path}|]

type instance PyFClassify (P.Key _a) = 'PyFString
instance {-# OVERLAPPING #-} (Coercible (P.Key a) UUID) => PyFToString (P.Key a) where
  pyfToString = U.toString . coerce @(P.Key a) @UUID

type Doc (str :: Symbol) (a :: Type) = a
