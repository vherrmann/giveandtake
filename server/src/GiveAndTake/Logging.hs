module GiveAndTake.Logging where

import GiveAndTake.Prelude hiding (MonadLogger)
import GiveAndTake.Utils (getUTCTime)
import System.IO (stderr)

runCTStderrLoggingT :: LoggingT m a -> m a
runCTStderrLoggingT =
  ( `runLoggingT`
      \loc logsrc loglev logstr -> do
        ct <- getUTCTime
        defaultOutput stderr loc logsrc loglev ("{" <> show ct <> "} " <> logstr)
  )
