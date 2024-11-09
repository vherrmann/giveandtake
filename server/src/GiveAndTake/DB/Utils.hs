{-# LANGUAGE AllowAmbiguousTypes #-}

module GiveAndTake.DB.Utils where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Coerce (Coercible, coerce)
import Data.Data (Typeable)
import Data.Typeable (typeRep)
import Data.UUID (UUID)
import Database.Persist qualified as P
import Database.Persist.Sql qualified as PS
import GiveAndTake.DB.Types
import GiveAndTake.Logging (runCTStderrLoggingT)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant (ServerError (..))
import Servant.Server (err404)

-- Run a database operation, and lift the result into a Handler.
-- This minimises usage of IO operations in other functions
runDB :: (HasDBPool m, MonadIO m) => PS.SqlPersistT (ResourceT (LoggingT IO)) a -> m a
runDB a = do
  pool <- askM

  liftIO $
    runCTStderrLoggingT $
      runResourceT $ -- FIXME: use global logger

        -- FIXME: replace retryOnBusy by a proper locking mechanism
        PS.runSqlPool a pool

doMigration :: (HasDBPool m, MonadUnliftIO m) => m ()
doMigration = do
  pool <- askM

  runCTStderrLoggingT $
    runResourceT $ -- FIXME: use global logger
      PS.runSqlPool (PS.runMigration migrateAll) pool

updateSelect ::
  forall backend record (m :: Type -> Type).
  ( P.PersistRecordBackend record backend
  , MonadIO m
  , PS.PersistQueryWrite backend
  ) =>
  [P.Filter record] ->
  [P.SelectOpt record] ->
  [P.Update record] ->
  ReaderT backend m [Entity record]
updateSelect filterOpts opts updates = do
  entities <- P.selectList filterOpts opts
  P.updateWhere filterOpts updates
  pure entities

updateGetBy ::
  forall backend record (m :: Type -> Type).
  ( P.PersistUniqueRead backend
  , P.PersistRecordBackend record backend
  , P.PersistStoreWrite backend
  , MonadIO m
  ) =>
  P.Unique record ->
  [P.Update record] ->
  ReaderT backend m (Maybe (Entity record))
updateGetBy unique updates = do
  mEntity <- P.getBy unique
  for_ mEntity \entity -> P.update (entityKey entity) updates
  pure mEntity

insertUUID ::
  ( P.PersistEntityBackend record ~ P.BaseBackend backend
  , P.PersistStoreWrite backend
  , MonadIO m
  , P.PersistEntity record
  , Coercible (P.Key record) UUID
  ) =>
  record ->
  ReaderT backend m (P.Key record)
insertUUID d = do
  newUUID <- randomUUID
  P.insertKey (coerce newUUID) d
  pure $ coerce newUUID

typeName :: forall a. (Typeable a) => Text
typeName = show . typeRep $ Proxy @a

selectByKey ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , Typeable record
  , MonadLogger m
  ) =>
  [P.Key record] ->
  m [Entity record]
selectByKey keys = do
  friends <- for keys \key -> do
    mRecord <- runDB . P.get $ key
    pure $ maybeToRight key $ fmap (Entity key) mRecord
  let missing = lefts friends
  let typeStr = typeName @record
  unless (null missing) $ logWarn [fmt|"Missing {typeStr} with keys: {show @Text missing}"|]
  pure $ rights friends

-- throws Server Error
getByKeySE ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , MonadError ServerError m
  , Typeable record
  ) =>
  P.Key record ->
  m record
getByKeySE key =
  let typeStr = typeName @record
   in runDB (P.get key)
        >>= maybeToMErr (err404{errBody = [fmt|{typeStr} with key {show @Text key} not found|]})

getByKeySEEnt ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , MonadError ServerError m
  , Typeable record
  ) =>
  P.Key record ->
  m (Entity record)
getByKeySEEnt key = Entity key <$> getByKeySE @record @m key

assureByKeySE ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , MonadError ServerError m
  , Typeable record
  ) =>
  P.Key record ->
  m ()
assureByKeySE key = void $ getByKeySE @record @m key

getByUniqSE ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , MonadError ServerError m
  , Typeable record
  ) =>
  P.Unique record ->
  m (P.Entity record)
getByUniqSE unique =
  let typeStr = typeName @record
   in runDB (P.getBy unique)
        >>= maybeToMErr (err404{errBody = [fmt|{typeStr} not founwd|]})
