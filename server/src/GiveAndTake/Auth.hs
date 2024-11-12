module GiveAndTake.Auth where

import Database.Persist ((=.))
import Database.Persist qualified as P
import GiveAndTake.DB
import GiveAndTake.Handlers.Feed
import GiveAndTake.Notify
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import GiveAndTake.VerifyEmail.Utils

finishSignup :: (HasHandler m) => WithKey' User -> m ()
finishSignup user = do
  uconfig :: UConfig <- askM
  token <- randomUrlToken
  ct <- getUTCTime
  void $ runDB $ insertUUID Feed{user = user.key, token, fType = MainFeed, createdAt = ct}
  let feedUrl = fUrl uconfig user.key token
  notify
    [user]
    NewNotif
      { title = [fmt|Welcome to {uconfig.serviceName}! Use the feed!|]
      , content = NotifWelcome (NotifWelcomeMsg user.value.name feedUrl)
      , prio = NPMedium
      }

updateFullyAuthenticated :: (HasHandler m) => UserId -> m ()
updateFullyAuthenticated userId = do
  mUser <- runDB $ P.get userId
  case mUser of
    Nothing -> logWarn [fmt|User {userId} not found.|]
    Just user -> do
      unless user.fullyAuthenticated do
        whenM (userHasConfirmedEmail userId) do
          finishSignup (WithKey userId user)

          runDB $ P.update userId [UserFullyAuthenticated =. True]
