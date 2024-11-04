module GiveAndTake.Handlers.Notif where

import Database.Persist ((<-.), (=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant (type (:<|>) (..))

getNotifs :: (HasHandler m) => UserId -> m [WithKey' Notification]
getNotifs userId = do
  notifs <- runDB $ P.selectList [NotificationUser ==. userId] [P.Desc NotificationCreatedAt]
  pure $ entityToWithKey <$> notifs

postNotifRead :: (HasHandler m) => [NotificationId] -> m ()
postNotifRead notifIds = do
  runDB $ P.updateWhere [NotificationId <-. notifIds] [NotificationRead =. True]

notifHandler :: Entity User -> RServer m NotifApi
notifHandler userEnt = getNotifs userEnt.key :<|> postNotifRead
