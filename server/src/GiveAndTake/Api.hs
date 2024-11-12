{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module GiveAndTake.Api where

import Data.Time (UTCTime)
import Database.Persist
import GiveAndTake.DB.Types as DB
import GiveAndTake.Prelude
import GiveAndTake.Servant.XML
import GiveAndTake.Types
import Servant (JSON, (:<|>), (:>))
import Servant qualified as S
import Servant.Auth (Auth, Cookie)
import Servant.Auth.Server (SetCookie)
import Servant.Multipart (FromMultipart, MultipartForm)
import Servant.Multipart qualified as SM
import Text.Feed.Types qualified as Feed
import Prelude qualified as P

--- Api Utils

--- Api data types

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
  , media :: [MediaId]
  , body :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VerifyEmail = VerifyEmail
  { id :: EmailConfirmId
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
  { mediaIds :: [MediaId]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SuccessLoginResponse = SuccessLoginResponse
  { userId :: UserId
  , user :: DB.User
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Public User Information
data UserPublic = UserPublic
  { name :: Text
  , createdAt :: UTCTime
  , avatar :: Maybe MediaId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

userToPublic :: DB.User -> UserPublic
userToPublic DB.User{..} = UserPublic{name, createdAt, avatar}

userEToWPublic :: Entity DB.User -> WithKey User UserPublic
userEToWPublic ent = WithKey ent.key $ userToPublic ent.val

data CheckResponse = CheckResponse
  { user :: DB.User
  , userId :: UserId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FeedUrlPostResponse = FeedUrlPostResponse
  { feedUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FriendsRequestGetResponse = FriendsRequestGetResponse
  { requestsToYou :: [WithKey User UserPublic]
  , requestsFromYou :: [WithKey User UserPublic]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ViewablePostData = ViewablePostData
  { post :: DB.Post
  , liked :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LockedHiddenPostData = LockedHiddenPostData
  { title :: Text
  , user :: UserId
  , createdAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UnlockedHiddenPostData = UnlockedHiddenPostData
  { post :: ViewablePostData
  , unlockedWithPost :: WithKey Post Post
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HiddenPostData = LockedHiddenPost LockedHiddenPostData | UnlockedHiddenPost UnlockedHiddenPostData
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UnhiddenPostData = UnhiddenPostData
  { post :: ViewablePostData
  , usedToUnlock :: [WithKey Post Post] -- should only be nonempty if the post is requested by the posts owner
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeletedPostData = DeletedPostData {user :: UserId, createdAt :: UTCTime}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiPost = HiddenPost HiddenPostData | UnhiddenPost UnhiddenPostData | DeletedPost DeletedPostData
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
  { group :: GroupId
  , user :: UserId
  , role :: DB.GroupRole
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiGroupMember = ApiGroupMember
  { user :: UserPublic
  , role :: DB.GroupRole
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiGroup = ApiGroup
  { group :: DB.Group
  , members :: [WithKey User ApiGroupMember]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiUserSettings = ApiUserSettings {name :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangePassword = ChangePassword {newPassword :: Text, oldPassword :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangeEmailAddress = ChangeEmailAddress {newEmail :: Text, password :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UploadAvatar = UploadAvatar
  { file :: SM.FileData SM.Tmp
  }
  deriving stock (Show, Generic)

instance FromMultipart SM.Tmp UploadAvatar where
  fromMultipart multipartData = pure UploadAvatar{file = P.head multipartData.files}

data EmailVerificationRequest = EmailVerificationRequest {email :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

--- Api Types

type PostsLikeApi =
  "like"
    :> ( (S.Capture "id" PostId :> S.Put '[JSON] ())
          :<|> (S.Capture "id" PostId :> S.Delete '[JSON] ())
       )

type PostsApi =
  "posts"
    :> ( (S.Capture "id" PostId :> S.Get '[JSON] ApiPost)
          :<|> (S.Capture "id" PostId :> S.Delete '[JSON] ())
          :<|> (S.ReqBody '[JSON] NewPost :> S.Post '[JSON] PostId)
          :<|> ("tradeables" :> S.Capture "user" UserId :> S.Get '[JSON] [WithKey Post DB.Post])
          :<|> ("trade" :> S.Capture "withPost" PostId :> S.Capture "forPost" PostId :> S.Post '[JSON] ())
          :<|> ("feed" :> S.Get '[JSON] [WithKey Post ApiPost])
          :<|> PostsLikeApi
       )

type UserSettingsApi =
  "settings"
    :> ( ("basic" :> (S.Get '[JSON] ApiUserSettings :<|> (S.ReqBody '[JSON] ApiUserSettings :> S.Put '[JSON] ())))
          :<|> ("password" :> S.ReqBody '[JSON] ChangePassword :> S.Put '[JSON] ())
          :<|> ("email" :> S.ReqBody '[JSON] ChangeEmailAddress :> S.Put '[JSON] (Maybe JobId))
          :<|> ("avatar" :> MultipartForm SM.Tmp UploadAvatar :> S.Post '[JSON] JobId)
          :<|> ("avatar" :> S.Delete '[JSON] ())
       )

type UsersApi =
  "users"
    :> ( (S.Capture "id" UserId :> S.Get '[JSON] UserPublic)
          :<|> (S.Capture "id" UserId :> "posts" :> S.Get '[JSON] [WithKey Post ApiPost])
          :<|> UserSettingsApi
       )

type MediaApi =
  "media"
    :> ( ( "upload"
            :> MultipartForm SM.Tmp UploadMedia
            :> S.Post '[JSON] JobId
         )
          :<|> (S.Capture "id" MediaId :> S.RawM) -- Should always be the last route (RawM catches everything)
       )

type FriendsApi =
  "friends"
    :> ( S.Get '[JSON] [WithKey User UserPublic]
          :<|> (S.Capture "friendId" UserId :> S.Delete '[JSON] ())
          :<|> ( "request"
                  :> ( S.Get '[JSON] FriendsRequestGetResponse
                        :<|> (S.Capture "friendId" UserId :> S.Post '[JSON] ())
                        :<|> (S.Capture "friendId" UserId :> "cancel" :> S.Post '[JSON] ())
                        :<|> (S.Capture "friendId" UserId :> "accept" :> S.Post '[JSON] ())
                        :<|> (S.Capture "friendId" UserId :> "reject" :> S.Post '[JSON] ())
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
    :> ( S.Get '[JSON] [WithKey Notification Notification]
          :<|> ("read" :> S.ReqBody '[JSON] [NotificationId] :> S.Post '[JSON] ())
       )

type JobApi =
  "job"
    :> ( S.Capture "id" JobId :> "status" :> S.Get '[JSON] DB.JobStatus
          :<|> ( S.Capture "id" JobId
                  :> "result"
                  :> ( ("verifyEmail" :> S.Get '[JSON] ())
                        :<|> ("mediaCompress" :> S.Get '[JSON] UploadMediaResponse)
                     )
               )
               -- :<|> S.Capture "id" JobId :> "cancel" :> S.Post '[JSON] ()
       )

type GroupsApi =
  "groups"
    :> ( S.Get '[JSON] [WithKey Group Group] -- FIXME: change to ApiGroup
          :<|> (S.Capture "id" GroupId :> "public" :> S.Get '[JSON] GroupPublic)
          :<|> (S.Capture "id" GroupId :> S.Get '[JSON] ApiGroup)
          :<|> (S.Capture "id" GroupId :> S.Delete '[JSON] ())
          :<|> (S.ReqBody '[JSON] NewGroup :> S.Post '[JSON] GroupId)
          :<|> ( "request"
                  :> ( S.Get '[JSON] [GroupId]
                        :<|> (S.Capture "id" GroupId :> S.Get '[JSON] [WithKey User UserPublic])
                        :<|> (S.Capture "id" GroupId :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupId :> "cancel" :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupId :> S.Capture "userId" UserId :> "accept" :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupId :> S.Capture "userId" UserId :> "reject" :> S.Post '[JSON] ())
                     )
               )
          :<|> ("roles" :> (S.ReqBody '[JSON] ChangeGroupRole :> S.Put '[JSON] ()))
          :<|> ( "member"
                  :> ( (S.Capture "id" GroupId :> "add" :> S.Capture "userId" UserId :> S.Post '[JSON] ())
                        :<|> (S.Capture "id" GroupId :> "remove" :> S.Capture "userId" UserId :> S.Delete '[JSON] ())
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
                  :> S.Post '[JSON] JobId
               )
          :<|> ( "verifyemail"
                  :> ( ( "request"
                          :> S.ReqBody '[JSON] EmailVerificationRequest
                          :> S.Post '[JSON] ()
                       )
                        :<|> ( "finish"
                                :> S.ReqBody '[JSON] VerifyEmail
                                :> S.Post '[JSON] ()
                             )
                     )
               )
          :<|> ("check" :> Auth '[Cookie] (Entity DB.User) :> S.Get '[JSON] CheckResponse)
       )

-- FIXME: add stuff to recreate link
type UnprotectedFeedApi =
  "feed"
    :> ( S.Header "Accept" Text
          :> S.Capture "id" UserId
          :> S.Capture "token" Text
          :> S.Get '[XML] Feed.Feed
       )

type UnprotectedApi = AuthApi :<|> UnprotectedFeedApi

type Api = "api" :> ((Auth '[Cookie] (Entity DB.User) :> ProtectedApi) :<|> UnprotectedApi)
