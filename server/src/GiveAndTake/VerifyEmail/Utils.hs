module GiveAndTake.VerifyEmail.Utils where

import Database.Persist ((==.))
import Database.Persist qualified as P
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types

userHasConfirmedEmail :: (HasHandler m) => UserId -> m Bool
userHasConfirmedEmail userId = do
  user <- getByKeySE userId
  runDB $
    P.exists @_ @_ @EmailConfirm
      [ #user ==. userId
      , #email ==. user.email
      , #isConfirmed ==. True
      ]
