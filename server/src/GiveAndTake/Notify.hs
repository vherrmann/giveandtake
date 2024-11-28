module GiveAndTake.Notify where

import GiveAndTake.DB
import GiveAndTake.Email
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils

data NewNotif = NewNotif
  { title :: Text
  , content :: NotifContent
  , prio :: NotifPrio
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

contentToEmailText :: UConfig -> NotifContent -> Text
contentToEmailText uconfig (NotifWelcome msg) =
  [fmtTrim|
Hello {msg.name}!

Welcome to {uconfig.serviceName}! Your individual feed can be accessed as an rss feed under the url:
{msg.url}
You can use this link with feed readers. For further information see https://docsgat.valentin-herrmann.de/feed/.
|]

dbNotify :: (HasHandler m) => [UserId] -> NewNotif -> m ()
dbNotify userIds newNotif = do
  ct <- getUTCTime
  -- check existence of users
  for_ userIds \userId ->
    runDB $
      insertUUID
        Notification
          { user = userId
          , title = newNotif.title
          , content = newNotif.content
          , prio = newNotif.prio
          , read = False
          , createdAt = ct
          }

emailNotify :: (HasHandler m) => [User] -> NewNotif -> m ()
emailNotify users newNotif = do
  uconfig <- askM @UConfig
  sendMail $
    Mail
      { to = users
      , subject = [fmt|New message: {newNotif.title}|]
      , plainBody = contentToEmailText uconfig newNotif.content
      , htmlBody = Nothing
      }

notify :: (HasHandler m) => [WithKey' User] -> NewNotif -> m ()
notify users newNotif = do
  dbNotify (users <&> (.key)) newNotif
  when (newNotif.prio >= NPMedium) $
    emailNotify (users <&> (.value)) newNotif
