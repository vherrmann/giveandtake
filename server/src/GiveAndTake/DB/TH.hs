{-# LANGUAGE TemplateHaskell #-}

module GiveAndTake.DB.TH where

import Database.Persist.TH
import GiveAndTake.Prelude

mySqlSettings :: MkPersistSettings
mySqlSettings =
  sqlSettings
    { mpsFieldLabelModifier = flip const
    , mpsDeriveInstances = [''Eq, ''Show, ''ToJSON, ''FromJSON]
    }
