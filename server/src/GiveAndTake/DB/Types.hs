{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module GiveAndTake.DB.Types (module GiveAndTake.DB.Types, Entity (..)) where

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
import GiveAndTake.DB.TH (mySqlSettings)
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

data GroupRole = GroupRoleNoRole | GroupRoleAdmin
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "GroupRole"

instance Ord GroupRole where
  GroupRoleNoRole <= GroupRoleNoRole = True
  GroupRoleNoRole <= GroupRoleAdmin = True
  GroupRoleAdmin <= GroupRoleNoRole = False
  GroupRoleAdmin <= GroupRoleAdmin = True

share
  [ mkPersist mySqlSettings
  , mkEntityDefList "basicModels"
  ]
  [persistLowerCase|
Post
    Id UUID primary unique
    title Text
    media [MediaId]  -- FIXME: seperate out
    body Text
    user UserId
    deleted Bool
    createdAt UTCTime
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

Friends
    user1 UserId
    user2 UserId
    createdAt UTCTime
    UniqueFriends user1 user2
    deriving Generic

FriendRequest
    from UserId
    to UserId
    createdAt UTCTime
    UniqueFriendRequest from to
    deriving Generic

EmailConfirm
    user UserId
    isConfirmed Bool
    secretHash Text
    confirmedAt (Maybe UTCTime)
    count Int
    sentAt UTCTime
    UniqueEmailConfirmUser user
    deriving Generic

Media
    Id UUID primary unique
    user UserId
    mimeType Text
    isDraft Bool
    isCompressed Bool
    createdAt UTCTime
    deriving Generic

Feed
    Id UUID primary unique
    user UserId
    token Text
    fType FeedType
    createdAt UTCTime
    UniqueFeedToken token
    UniqueFeedKind user fType
    deriving Generic

Notification
    Id UUID primary unique
    user UserId
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

TradedPost
    post1 PostId
    user1 UserId
    post2 PostId
    user2 UserId
    createdAt UTCTime
    -- each post can only be traded once
    UniqueTradedPostPost1User2 post1 user2
    UniqueTradedPostPost2User1 post2 user1
    deriving Generic

Group
    Id UUID primary unique
    name Text
    owner UserId
    createdAt UTCTime
    deriving Generic

GroupMember
    group GroupId
    user UserId
    role GroupRole
    createdAt UTCTime
    UniqueGroupMember group user
    deriving Generic

GroupJoinRequest
    from UserId
    to GroupId
    createdAt UTCTime
    UniqueGroupJoinRequest from to
    deriving Generic

-- SessionConfig
SConfig
    cookieKey JWK
    deriving Generic
|]

--- Job data types

-- Media
data AllowedMediaTopTypes = MimeImage | MimeVideo
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MediaUploadFile = MediaUploadFile {name :: Text, mediaId :: MediaId, cType :: AllowedMediaTopTypes}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GATJobMediaUploadData = GATJobMediaUploadData {files :: [MediaUploadFile], userId :: UserId}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Email verification
data GATJobVerifyEmailData = GATJobVerifyEmailData
  { secret :: Text
  , userId :: UserId
  , userName :: Text
  , userEmail :: Text
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "GATJobVerifyEmailData"

--- JOB Queue
data GATJob
  = GATJobVerifyEmail GATJobVerifyEmailData
  | GATJobMediaUpload GATJobMediaUploadData
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "GATJob"

data JobResult
  = GATJobResultVerifyEmail ()
  | GATJobResultMediaUpload [MediaId]
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
  [ -- mkPersistWith mySqlSettings $(discoverEntities)
    mkPersist mySqlSettings
  , mkEntityDefList "jobModels"
  ]
  [persistLowerCase|
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
|]

migrateAll :: PS.Migration
migrateAll = migrateModels (basicModels <> jobModels)

-- FIXME: replace with a more general solution
deriving stock instance Generic (P.Key User)
instance FromJSON (Entity User)
instance FromJWT (Entity User)
instance ToJSON (Entity User)
instance ToJWT (Entity User)

entityToWithKey :: Entity a -> WithKey' a
entityToWithKey (Entity k v) = WithKey k v

-- shorthands

instance HasField "key" (Entity a) (P.Key a) where
  getField = entityKey

instance HasField "val" (Entity a) a where
  getField = entityVal
