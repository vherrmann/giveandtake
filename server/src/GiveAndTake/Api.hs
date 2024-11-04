{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module GiveAndTake.Api where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Persist
import GiveAndTake.DB.Types qualified as DB
import GiveAndTake.Prelude
import GiveAndTake.Servant.XML
import GiveAndTake.Types
import Servant (JSON, (:<|>), (:>))
import Servant qualified as S
import Servant.Auth (Auth, Cookie)
import Servant.Auth.Server (SetCookie)
import Servant.Multipart (FromMultipart, MultipartForm)
import Servant.Multipart qualified as SM
import Text.Feed.Types (Feed)

--- Api Utils

--- Api data types

type UserUUID = UUID
type PostUUID = UUID
type MediaUUID = UUID
type FeedUUID = UUID
type NotifUUID = UUID
type JobUUID = UUID
type GroupUUID = UUID

data LoginData = LoginData
  { email :: Text
  , password :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SignupData = SignupData
  { name :: Text
  , email :: Text
  , secret :: Text
  , password :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NewPost = NewPost
  { title :: Text
  , media :: [PostUUID]
  , body :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VerifyEmail = VerifyEmail
  { user :: UserUUID
  , secret :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UploadMedia = UploadMedia
  { files :: [SM.FileData SM.Tmp]
  }
  deriving stock (Show, Generic)

instance FromMultipart SM.Tmp UploadMedia where
  fromMultipart multipartData = pure UploadMedia{files = multipartData.files}

data UploadMediaResponse = UploadMediaResponse
  { mediaIds :: [MediaUUID]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SuccessLoginResponse = SuccessLoginResponse
  { userId :: UserUUID
  , user :: DB.User
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Public User Information
data UserPublic = UserPublic
  { name :: Text
  , createdAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

userToPublic :: DB.User -> UserPublic
userToPublic DB.User{..} = UserPublic{name, createdAt}

data CheckResponse = CheckResponse
  { user :: DB.User
  , userId :: UserUUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FeedUrlPostResponse = FeedUrlPostResponse
  { feedUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FriendsRequestGetResponse = FriendsRequestGetResponse
  { requestsToYou :: [WithUUID UserPublic]
  , requestsFromYou :: [WithUUID UserPublic]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LockedHiddenPostData = LockedHiddenPostData
  { title :: Text
  , user :: UUID
  , createdAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UnlockedHiddenPostData = UnlockedHiddenPostData
  { post :: DB.Post
  , unlockedWithPost :: WithUUID DB.Post
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HiddenPostData = LockedHiddenPost LockedHiddenPostData | UnlockedHiddenPost UnlockedHiddenPostData
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UnhiddenPostData = UnhiddenPostData
  { post :: DB.Post
  , usedToUnlock :: [WithUUID DB.Post] -- should only be nonempty if the post is requested by the posts owner
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiPost = HiddenPost HiddenPostData | UnhiddenPost UnhiddenPostData
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GroupPublic = GroupPublic
  { name :: Text
  , createdAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NewGroup = NewGroup
  { name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangeGroupRole = ChangeGroupRole
  { group :: GroupUUID
  , user :: UserUUID
  , role :: DB.GroupRole
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiGroup = ApiGroup
  { group :: DB.Group
  , members :: [WithUUID UserPublic]
  , admins :: [WithUUID UserPublic]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

--- Api Types

type PostsApi =
  "posts"
    :> ( (S.Capture "id" PostUUID :> S.Get '[JSON] ApiPost)
          :<|> (S.Capture "id" PostUUID :> S.Delete '[JSON] ())
          :<|> (S.ReqBody '[JSON] NewPost :> S.Post '[JSON] PostUUID)
          :<|> ("tradeables" :> S.Capture "user" UserUUID :> S.Get '[JSON] [WithUUID DB.Post])
          :<|> ("trade" :> S.Capture "withPost" PostUUID :> S.Capture "forPost" PostUUID :> S.Post '[JSON] ())
          :<|> ("feed" :> S.Get '[JSON] [WithUUID ApiPost])
       )

type UsersApi =
  "users"
    :> ( (S.Capture "id" UserUUID :> S.Get '[JSON] UserPublic)
          :<|> (S.Capture "id" UserUUID :> "posts" :> S.Get '[JSON] [WithUUID ApiPost])
       )

type MediaApi =
  "media"
    :> ( ( "upload"
            :> MultipartForm SM.Tmp UploadMedia
            :> S.Post '[JSON] JobUUID
         )
          :<|> (S.Capture "id" MediaUUID :> S.RawM) -- Should always be the last route (RawM catches everything)
       )

type FriendsApi =
  "friends"
    :> ( S.Get '[JSON] [WithUUID UserPublic]
          :<|> (S.Capture "friendId" UserUUID :> S.Delete '[JSON] ())
          :<|> ( "request"
                  :> ( S.Get '[JSON] FriendsRequestGetResponse
                        :<|> (S.Capture "friendId" UserUUID :> S.Post '[JSON] ())
                        :<|> (S.Capture "friendId" UserUUID :> "accept" :> S.Post '[JSON] ())
                        :<|> (S.Capture "friendId" UserUUID :> "reject" :> S.Post '[JSON] ())
                     )
               )
       )

type ProtectedFeedApi =
  "feed"
    :> "url"
    :> S.ReqBody '[JSON] DB.FeedType
    :> S.Post '[JSON] FeedUrlPostResponse -- Returns the feed url, creates feed if it didn't exist

type NotifApi =
  "notif"
    :> ( S.Get '[JSON] [WithUUID DB.Notification]
          :<|> ("read" :> S.ReqBody '[JSON] [NotifUUID] :> S.Post '[JSON] ())
       )

type JobApi =
  "job"
    :> ( S.Capture "id" JobUUID :> "status" :> S.Get '[JSON] DB.JobStatus
          :<|> ( S.Capture "id" JobUUID
                  :> "result"
                  :> ( ("verifyEmail" :> S.Get '[JSON] ())
                        :<|> ("mediaCompress" :> S.Get '[JSON] UploadMediaResponse)
                     )
               )
               -- :<|> S.Capture "id" JobUUID :> "cancel" :> S.Post '[JSON] ()
       )

type GroupsApi =
  "groups"
    :> ( S.Get '[JSON] [WithUUID DB.Group]
          :<|> (S.Capture "id" GroupUUID :> "public" :> S.Get '[JSON] GroupPublic)
          :<|> (S.Capture "id" GroupUUID :> S.Get '[JSON] ApiGroup)
          :<|> (S.Capture "id" GroupUUID :> S.Delete '[JSON] ())
          :<|> (S.ReqBody '[JSON] NewGroup :> S.Post '[JSON] GroupUUID)
          :<|> ( "request"
                  :> ( S.Get '[JSON] [GroupUUID]
                        :<|> (S.Capture "id" GroupUUID :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupUUID :> "cancel" :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupUUID :> S.Capture "userId" UserUUID :> "accept" :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupUUID :> S.Capture "userId" UserUUID :> "reject" :> S.Post '[JSON] ())
                     )
               )
          :<|> ("roles" :> (S.ReqBody '[JSON] ChangeGroupRole :> S.Post '[JSON] ()))
          :<|> ( "member"
                  :> ( (S.Capture "id" GroupUUID :> "add" :> S.Capture "userId" UserUUID :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupUUID :> "remove" :> S.Capture "userId" UserUUID :> S.Delete '[JSON] ())
                     )
               )
       )

type ProtectedApi =
  PostsApi
    :<|> UsersApi
    :<|> MediaApi
    :<|> FriendsApi
    :<|> ProtectedFeedApi
    :<|> NotifApi
    :<|> JobApi
    :<|> GroupsApi

type AuthApi =
  "auth"
    :> ( ( "login"
            :> S.ReqBody '[JSON] LoginData
            :> S.Verb
                'S.POST
                200
                '[JSON]
                ( S.Headers
                    '[ S.Header "Set-Cookie" SetCookie
                     , S.Header "Set-Cookie" SetCookie
                     ]
                    SuccessLoginResponse
                )
         )
          :<|> ( "logout"
                  :> S.Verb
                      'S.POST
                      204
                      '[JSON]
                      ( S.Headers
                          '[ S.Header "Set-Cookie" SetCookie
                           , S.Header "Set-Cookie" SetCookie
                           ]
                          S.NoContent
                      )
               )
          :<|> ( "signup"
                  :> S.ReqBody '[JSON] SignupData
                  :> S.Post '[JSON] JobUUID
               )
          :<|> ( "verifyemail"
                  :> S.ReqBody '[JSON] VerifyEmail
                  :> S.Post '[JSON] ()
               )
          :<|> ("check" :> Auth '[Cookie] (Entity DB.User) :> S.Get '[JSON] CheckResponse)
       )

-- FIXME: add stuff to recreate link
type UnprotectedFeedApi =
  "feed"
    :> ( S.Header "Accept" Text
          :> S.Capture "id" UserUUID
          :> S.Capture "token" Text
          :> S.Get '[XML] Feed
       )

type UnprotectedApi = AuthApi :<|> UnprotectedFeedApi

type Api = "api" :> ((Auth '[Cookie] (Entity DB.User) :> ProtectedApi) :<|> UnprotectedApi)
