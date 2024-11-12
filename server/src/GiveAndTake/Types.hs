{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GiveAndTake.Types where

import Control.Monad.Catch (Exception)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Logger.CallStack qualified as L
import Data.Aeson qualified as A
import Data.Pool (Pool)
import Database.Persist qualified as P
import Database.Persist.Sql qualified as PS
import GHC.Stack (HasCallStack)
import GiveAndTake.Job.Con (HasJobCon, JobCon)
import GiveAndTake.Prelude
import Servant
import System.IO (FilePath)
import Text.Show (Show (show))
import Text.Show qualified as TS

data SmtpMethod = SmtpStartTLS | SmtpSSL | SmtpPlain
  deriving stock (Show, Generic)

-- remove smtp prefix
smtpAesonOptions :: A.Options
smtpAesonOptions = A.defaultOptions{A.constructorTagModifier = drop 4}

instance FromJSON SmtpMethod where
  parseJSON = A.genericParseJSON smtpAesonOptions

data EmailConfig = EmailConfig
  { smtpHost :: Text
  , smtpPort :: Integer
  , smtpUser :: Text
  , smtpPassFile :: FilePath
  , smtpFrom :: Text
  , smtpMethod :: SmtpMethod
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data DBConfig = DBConfig
  { connections :: Int
  , connectionString :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- UserConfig
data UConfig = UConfig
  { mediaDir :: Text
  , host :: Text
  , port :: Int
  , baseUrl :: Text
  , docsBaseUrl :: Text
  , serviceName :: Text
  , emailConfig :: EmailConfig
  , dbConfig :: DBConfig
  , timeout :: Int -- Timeout of requests in seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data DynUConfig = DynUConfig {smtpPass :: Text}
  deriving stock (Show, Generic)

data WithKey a b = WithKey
  { key :: P.Key a
  , value :: b
  }
  deriving stock (Generic, Functor)

instance (Show (P.Key a), Show b) => Show (WithKey a b) where
  show WithKey{..} = "WithKey {key = " <> TS.show key <> ", value = " <> TS.show value <> "}"

instance (FromJSON (P.Key a), FromJSON b) => FromJSON (WithKey a b) where
  parseJSON = A.genericParseJSON A.defaultOptions

instance (ToJSON (P.Key a), ToJSON b) => ToJSON (WithKey a b) where
  toEncoding = A.genericToEncoding A.defaultOptions

type WithKey' a = WithKey a a

type HasUConfig m = (MonadReaderM UConfig m, MonadReaderM DynUConfig m)
type HasDBPool m = (MonadReaderM (Pool PS.SqlBackend) m)

type HasHandler m =
  ( HasCallStack
  , HasUConfig m
  , HasDBPool m
  , MonadLogger m
  , MonadError ServerError m
  , MonadIO m
  , HasJobCon m
  )

newtype RHandler m a = RHandler
  {_unRHandler :: (HasHandler m) => m a}

unRHandler :: forall (m :: Type -> Type) a. (HasHandler m) => RHandler m a -> m a
unRHandler (RHandler m) = m

instance Functor (RHandler m) where
  fmap f (RHandler m) = RHandler (fmap f m)

instance Applicative (RHandler m) where
  pure a = RHandler (pure a)
  (RHandler f) <*> (RHandler a) = RHandler (f <*> a)

instance Monad (RHandler m) where
  (RHandler a) >>= f = RHandler (a >>= \x -> unRHandler (f x))

instance MonadReaderM UConfig (RHandler m) where
  askM = RHandler askM
  localM f r = RHandler $ localM f $ unRHandler r
  readerM g = RHandler $ readerM g

instance MonadReaderM DynUConfig (RHandler m) where
  askM = RHandler askM
  localM f r = RHandler $ localM f $ unRHandler r
  readerM g = RHandler $ readerM g

instance MonadReaderM JobCon (RHandler m) where
  askM = RHandler askM
  localM f r = RHandler $ localM f $ unRHandler r
  readerM g = RHandler $ readerM g

instance MonadReaderM (Pool PS.SqlBackend) (RHandler m) where
  askM = RHandler askM
  localM f r = RHandler $ localM f $ unRHandler r
  readerM g = RHandler $ readerM g

instance L.MonadLogger (RHandler m) where
  monadLoggerLog loc logSrc loglvl msg = RHandler $ monadLoggerLog loc logSrc loglvl msg

instance MonadIO (RHandler m) where
  liftIO io = RHandler $ liftIO io

instance MonadError ServerError (RHandler m) where
  throwError e = RHandler $ throwError e
  catchError (RHandler m) f = RHandler $ catchError m (unRHandler . f)

-- instance MonadThrow (RHandler m) where
--   throwM e = RHandler $ throwM e

-- instance MonadCatch (RHandler m) where
--   catch (RHandler m) f = RHandler $ catch m (unRHandler . f)

-- instance MonadMask (RHandler m) where
--   mask f = RHandler $ mask (\u -> unRHandler (f (\m -> RHandler (u (unRHandler m)))))

--   uninterruptibleMask a = RHandler $ uninterruptibleMask (\u -> unRHandler (a (\m -> RHandler (u (unRHandler m)))))
--   generalBracket acquire release use = RHandler $ generalBracket (unRHandler acquire) (\x y -> unRHandler $ release x y) (\x -> unRHandler $ use x)

type RServer m api = ServerT api (RHandler m)

newtype JobError = JobError {jobError :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Exception JobError
