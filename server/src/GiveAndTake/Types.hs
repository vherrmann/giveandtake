{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GiveAndTake.Types where

import Control.Monad.Catch (MonadCatch (..), MonadMask (..), MonadThrow (..))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger.CallStack qualified as L
import Data.Pool (Pool)
import Data.UUID
import Database.Persist.Sql qualified as PS
import GHC.Stack (HasCallStack)
import GiveAndTake.Prelude
import Servant
import Servant qualified as S

data EmailConfig = EmailConfig
  { smtpHost :: Text
  , smtpPort :: Integer
  , smtpUser :: Text
  , smtpPass :: Text
  , smtpFrom :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- UserConfig
data UConfig = UConfig
  { dbPath :: Text
  , dbConnections :: Int
  , mediaDir :: Text
  , host :: Text -- FIXME: unused
  , port :: Int -- FIXME: unused
  , authority :: Text
  , serviceName :: Text
  , emailConfig :: EmailConfig
  , timeout :: Int -- Timeout of requests in seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data WithUUID a = WithUUID
  { uuid :: UUID
  , value :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type HasUConfig m = (MonadReaderM UConfig m)
type HasDBPool m = (MonadReaderM (Pool PS.SqlBackend) m)

type HasHandler m =
  ( HasCallStack
  , HasUConfig m
  , HasDBPool m
  , MonadLogger m
  , MonadError ServerError m
  , MonadIO m
  , MonadCatch m
  , MonadMask m
  )

newtype RHandler m a = RHandler
  {_unRHandler :: (HasHandler m) => m a}

unRHandler :: forall (m :: Type -> Type) a. (HasHandler m) => RHandler m a -> m a
unRHandler m = _unRHandler m

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

instance MonadThrow (RHandler m) where
  throwM e = RHandler $ throwM e

instance MonadCatch (RHandler m) where
  catch (RHandler m) f = RHandler $ catch m (unRHandler . f)

instance MonadMask (RHandler m) where
  mask f = RHandler $ mask (\u -> unRHandler (f (\m -> RHandler (u (unRHandler m)))))

  uninterruptibleMask a = RHandler $ uninterruptibleMask (\u -> unRHandler (a (\m -> RHandler (u (unRHandler m)))))
  generalBracket acquire release use = RHandler $ generalBracket (unRHandler acquire) (\x y -> unRHandler $ release x y) (\x -> unRHandler $ use x)

type RServer m api = ServerT api (RHandler m)
