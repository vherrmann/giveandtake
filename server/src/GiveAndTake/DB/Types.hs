{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module GiveAndTake.DB.Types (module GiveAndTake.DB.Types, Entity (..)) where

import Control.Lens (Lens')
import Crypto.JOSE (JWK)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as U
import Database.Persist (Entity (..))
import Database.Persist qualified as P
import Database.Persist.Sql qualified as PS
import Database.Persist.TH
import GHC.Records (HasField (..))
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant.Auth.JWT (FromJWT)
import Servant.Auth.Server (ToJWT)
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

data GroupRole = GroupRoleNoRole | GroupRoleAdmin
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "GroupRole"

instance Ord GroupRole where
  GroupRoleNoRole <= GroupRoleNoRole = True
  GroupRoleNoRole <= GroupRoleAdmin = True
  GroupRoleAdmin <= GroupRoleNoRole = False
  GroupRoleAdmin <= GroupRoleAdmin = True

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
    deleted Bool
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

TradedPost
    post1 UUID
    user1 UUID
    post2 UUID
    user2 UUID
    createdAt UTCTime
    -- each post can only be traded once
    UniqueTradedPostPost1User2 post1 user2
    UniqueTradedPostPost2User1 post2 user1
    deriving Generic

Group
    Id UUID primary unique
    name Text
    owner UUID
    createdAt UTCTime
    deriving Generic

GroupMember
    group UUID
    user UUID
    role GroupRole
    createdAt UTCTime
    UniqueGroupMember group user
    deriving Generic

GroupJoinRequest
    from UUID
    to UUID
    createdAt UTCTime
    UniqueGroupJoinRequest from to
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
instance ToKeyConstr Group where
  type KeyType Group = UUID
  packKey = GroupKey
  unpackKey (GroupKey x) = x

entityToWithUUID :: (ToKeyConstr a, KeyType a ~ UUID) => Entity a -> WithUUID a
entityToWithUUID (Entity k v) = WithUUID (unpackKey k) v

-- shorthands

instance (ToKeyConstr a, KeyType a ~ b) => HasField "key" (Entity a) b where
  getField = entityUKey

instance HasField "val" (Entity a) a where
  getField = entityVal
