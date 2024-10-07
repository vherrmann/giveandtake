module GiveAndTake.Handlers.Notif where

import Database.Persist ((<-.), (=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant (type (:<|>) (..))

getNotifs :: (HasHandler m) => UserUUID -> m [WithUUID Notification]
getNotifs userId = do
  notifs <- runDB $ P.selectList [NotificationUser ==. userId] [P.Desc NotificationCreatedAt]
  pure $ entityToWithUUID <$> notifs

postNotifRead :: (HasHandler m) => [NotifUUID] -> m ()
postNotifRead notifIds = do
  runDB $ P.updateWhere [NotificationId <-. fmap packKey notifIds] [NotificationRead =. True]

notifHandler :: Entity User -> RServer m NotifApi
notifHandler userEnt = getNotifs userEnt.key :<|> postNotifRead
