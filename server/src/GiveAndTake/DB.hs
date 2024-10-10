{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module GiveAndTake.DB (module GiveAndTake.DB, Entity (..)) where

import Control.Lens (Lens')
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Crypto.JOSE (JWK)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as B
import Data.Data (Typeable)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime)
import Data.Typeable (typeRep)
import Data.UUID (UUID)
import Data.UUID qualified as U
import Database.Persist (Entity (..))
import Database.Persist qualified as P
import Database.Persist.Sql qualified as PS
import Database.Persist.TH
import GHC.Records (HasField (..))
import GiveAndTake.Logging (runCTStderrLoggingT)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant (ServerError (..))
import Servant.Auth.JWT (FromJWT)
import Servant.Auth.Server (ToJWT)
import Servant.Server (err404)
import Text.Read (Read)
import Web.PathPieces (PathPiece (..))

instance P.PersistField UUID where
  toPersistValue uuid =
    P.PersistText (U.toText uuid)
  fromPersistValue pv =
    case pv of
      P.PersistText txt -> maybeToRight "UUID decoding failed" (U.fromText txt)
      _ -> Left "Bad data in database"

instance PS.PersistFieldSql UUID where
  sqlType _ = P.SqlString

instance PathPiece UUID where
  fromPathPiece = U.fromString . T.unpack
  toPathPiece = T.pack . U.toString

instance P.PersistField JWK where
  toPersistValue jwk =
    P.PersistText (P.toJsonText jwk)
  fromPersistValue pv =
    case pv of
      P.PersistText txt -> maybeToRight "JWK decoding failed" . A.decode . B.fromStrict . T.encodeUtf8 $ txt
      _ -> Left "Bad data in database"

instance PS.PersistFieldSql JWK where
  sqlType _ = P.SqlString

-- Can be extended later
data FeedType = MainFeed
  deriving stock (Eq, Show, Read, Generic)

customAesonOptions :: A.Options
customAesonOptions = A.defaultOptions{A.tagSingleConstructors = True}

instance FromJSON FeedType where
  parseJSON = A.genericParseJSON customAesonOptions

instance ToJSON FeedType where
  toEncoding = A.genericToEncoding customAesonOptions

derivePersistField "FeedType"

data NotifPrio = NPLow | NPMedium | NPHigh
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "NotifPrio"

data NotifWelcomeMsg = NotifWelcomeMsg
  { name :: Text
  , url :: Text
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "NotifWelcomeMsg"

data NotifContent = NotifWelcome NotifWelcomeMsg
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "NotifContent"

data AuthCodeType = ACTSignup
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "AuthCodeType"

--- Job data types
-- Email verification
data GATJobVerifyEmailData = GATJobVerifyEmailData {secret :: Text, userId :: UUID, userName :: Text, userEmail :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "GATJobVerifyEmailData"

-- Media
data AllowedMediaTopTypes = MimeImage | MimeVideo
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MediaUploadFile = MediaUploadFile {name :: Text, mediaId :: UUID, cType :: AllowedMediaTopTypes}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GATJobMediaUploadData = GATJobMediaUploadData {files :: [MediaUploadFile], userId :: UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

--- JOB Queue
data GATJob
  = GATJobVerifyEmail GATJobVerifyEmailData
  | GATJobMediaUpload GATJobMediaUploadData
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "GATJob"

data JobResult
  = GATJobResultVerifyEmail ()
  | GATJobResultMediaUpload [UUID]
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "JobResult"

data JobStatus = JobPending | JobRunning | JobFinished | JobFailed
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "JobStatus"

jobEnded :: [JobStatus]
jobEnded = [JobFinished, JobFailed]

share
  [ mkPersist
      ( sqlSettings
          { mpsFieldLabelModifier = flip const
          , mpsDeriveInstances = [''Eq, ''Show, ''ToJSON, ''FromJSON]
          }
      )
  , mkMigrate "migrateAll"
  , mkEntityDefList "entityDefs"
  ]
  [persistLowerCase|
Post
    Id UUID primary unique
    title Text
    media [UUID]
    body Text
    user UUID -- FIXME: seperate out
    -- author UUID
    createdAt UTCTime
    deriving Generic

Friends
    user1 UUID
    user2 UUID
    createdAt UTCTime
    UniqueFriends user1 user2
    deriving Generic

FriendRequest
    from UUID
    to UUID
    createdAt UTCTime
    UniqueFriendRequest from to
    deriving Generic

-- The User data might be included in the cookie of the user
User
    Id UUID primary unique
    name Text unique
    email Text unique
    -- emailConfirmed Bool
    passwordHash Text
    fullyAuthenticated Bool
    createdAt UTCTime

    UniqueUserName name
    UniqueUserEmail email
    deriving Generic ToJWT FromJWT

EmailConfirm
    user UUID
    isConfirmed Bool
    secretHash Text
    confirmedAt (Maybe UTCTime)
    count Int
    sentAt UTCTime
    UniqueEmailConfirmUser user
    deriving Generic

Media
    Id UUID primary unique
    user UUID
    mimeType Text
    isDraft Bool
    isCompressed Bool
    createdAt UTCTime
    deriving Generic

Feed
    Id UUID primary unique
    user UUID
    token Text
    fType FeedType
    createdAt UTCTime
    UniqueFeedToken token
    UniqueFeedKind user fType
    deriving Generic

Notification
    Id UUID primary unique
    user UUID
    title Text
    content NotifContent
    prio NotifPrio
    read Bool
    createdAt UTCTime
    deriving Generic

AuthCode
    authType AuthCodeType
    secret Text
    used Bool
    createdAt UTCTime
    UniqueAuthCode secret
    deriving Generic

Job
    Id UUID primary unique
    payload GATJob
    status JobStatus
    createdAt UTCTime
    startedAt (Maybe UTCTime)
    endedAt (Maybe UTCTime)
    jobError (Maybe Text)
    result (Maybe JobResult)
    deriving Generic

-- SessionConfig
SConfig
    cookieKey JWK
    deriving Generic
|]

deriving stock instance Generic (P.Key User)
instance FromJSON (Entity User)
instance FromJWT (Entity User)
instance ToJSON (Entity User)
instance ToJWT (Entity User)

class ToKeyConstr (a :: Type) where
  type KeyType a
  packKey :: KeyType a -> P.Key a
  unpackKey :: P.Key a -> KeyType a

entityUKey :: (ToKeyConstr a, KeyType a ~ b) => Entity a -> b
entityUKey = unpackKey . entityKey

entityUKeyLens :: (ToKeyConstr a, KeyType a ~ b) => Lens' (Entity a) b
entityUKeyLens f (Entity key val) = (\key -> Entity (packKey key) val) <$> f (unpackKey key)

entityValLens :: Lens' (Entity a) a
entityValLens f (Entity key val) = Entity key <$> f val

instance ToKeyConstr User where
  type KeyType User = UUID
  packKey = UserKey
  unpackKey (UserKey x) = x
instance ToKeyConstr Post where
  type KeyType Post = UUID
  packKey = PostKey
  unpackKey (PostKey x) = x
instance ToKeyConstr Media where
  type KeyType Media = UUID
  packKey = MediaKey
  unpackKey (MediaKey x) = x
instance ToKeyConstr Feed where
  type KeyType Feed = UUID
  packKey = FeedKey
  unpackKey (FeedKey x) = x
instance ToKeyConstr Notification where
  type KeyType Notification = UUID
  packKey = NotificationKey
  unpackKey (NotificationKey x) = x
instance ToKeyConstr Job where
  type KeyType Job = UUID
  packKey = JobKey
  unpackKey (JobKey x) = x

entityToWithUUID :: (ToKeyConstr a, KeyType a ~ UUID) => Entity a -> WithUUID a
entityToWithUUID (Entity k v) = WithUUID (unpackKey k) v

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
updateSelect filter opts updates = do
  entities <- P.selectList filter opts
  P.updateWhere filter updates
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
  , ToKeyConstr record
  , KeyType record ~ UUID
  ) =>
  record ->
  ReaderT backend m UUID
insertUUID d = do
  newUUID <- randomUUID
  P.insertKey (packKey newUUID) d
  pure newUUID

typeName :: forall a. (Typeable a) => Text
typeName = show . typeRep $ Proxy @a

selectByKey ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , ToKeyConstr record
  , Typeable record
  , MonadLogger m
  , Show (KeyType record)
  ) =>
  [KeyType record] ->
  m [Entity record]
selectByKey keys = do
  friends <- for keys \key -> do
    mRecord <- runDB . P.get . packKey @record $ key
    pure $ maybeToRight key $ fmap (Entity (packKey key)) mRecord
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
  , ToKeyConstr record
  , MonadError ServerError m
  , Typeable record
  , Typeable (KeyType record)
  , Show (KeyType record)
  ) =>
  KeyType record ->
  m record
getByKeySE key =
  let typeStr = typeName @record
      keyTypeStr = typeName @(KeyType record)
   in runDB (P.get $ packKey @record key)
        >>= maybeToMErr (err404{errBody = [fmt|{typeStr} with key {keyTypeStr} {show @Text key} not found|]})

assureByKeySE ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , ToKeyConstr record
  , KeyType record ~ UUID
  , MonadError ServerError m
  , Typeable record
  ) =>
  UUID ->
  m ()
assureByKeySE uuid = void $ getByKeySE @record @m uuid

-- shorthands

instance (ToKeyConstr a, KeyType a ~ b) => HasField "key" (Entity a) b where
  getField = entityUKey

instance HasField "val" (Entity a) a where
  getField = entityVal

-- instance {-# OVERLAPPING #-} (ToKeyConstr a, KeyType a ~ b) => HasField' "key" (Entity a) b where
--   field' = entityUKeyLens

-- instance {-# OVERLAPPING #-} HasField' "val" (Entity a) a where
--   field' = entityValLens
