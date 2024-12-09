module GiveAndTake.Logging where

import GiveAndTake.Prelude hiding (MonadLogger)
import GiveAndTake.Types (Verbosity (..))
import GiveAndTake.Utils (getUTCTime)
import System.IO (stderr)

verbosityToLogLev :: Verbosity -> LogLevel
verbosityToLogLev = \case
  VError -> LevelError
  VWarning -> LevelWarn
  VInfo -> LevelInfo
  VDebug -> LevelDebug

runCTStderrLoggingT :: Verbosity -> LoggingT m a -> m a
runCTStderrLoggingT v =
  ( `runLoggingT`
      \loc logsrc loglev logstr -> do
        ct <- getUTCTime
        when (loglev >= verbosityToLogLev v) $
          defaultOutput stderr loc logsrc loglev ("{" <> show ct <> "} " <> logstr)
  )
