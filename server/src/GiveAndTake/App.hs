{-# LANGUAGE NoFunctionalDependencies #-}

module GiveAndTake.App where

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.YAML (prettyPosWithSource)
import Data.YAML.Aeson qualified as Y
import Database.Persist qualified as P
import Database.Persist.Postgresql qualified as PS
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Fixes ()
import GiveAndTake.Handlers
import GiveAndTake.Job (jobRunner)
import GiveAndTake.JobCon (createJobCon)
import GiveAndTake.Logging
import GiveAndTake.Prelude
import GiveAndTake.Types
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Middleware.RequestLogger qualified as W
import Network.Wai.Middleware.Timeout qualified as W
import Servant qualified as S
import Servant.Auth.Server (CookieSettings (..))
import Servant.Auth.Server qualified as SA
import Servant.Server (Context ((:.)))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UnliftIO (concurrently_)

getSConfig :: (HasDBPool m, MonadIO m) => m SConfig
getSConfig = runDB do
  mConf <- P.selectFirst @_ @_ @SConfig [] []
  case mConf of
    Just conf -> pure (P.entityVal conf)
    Nothing -> do
      cookieKey <- liftIO SA.generateKey
      let sConfig = SConfig{cookieKey}
      -- let configCookieKey = B.toStrict (A.encode key)
      P.insert_ sConfig
      pure sConfig

getUConfig :: (MonadIO m) => [Char] -> m UConfig
getUConfig configPath = do
  -- FIXME: better error message
  f <- liftIO $ BL.readFile configPath
  case Y.decode1 f of
    Left (pos, msg) -> do
      liftIO $ hPutStrLn stderr $ prettyPosWithSource pos f [fmt| error while decoding {configPath}|] <> msg
      liftIO exitFailure
    Right c -> pure c

getDynUConfig :: (MonadIO m) => UConfig -> m DynUConfig
getDynUConfig uconfig = do
  smtpPass <- liftIO . fmap (T.decodeUtf8 . B.strip) . B.readFile $ uconfig.emailConfig.smtpPassFile
  pure $ DynUConfig{..}

type HasServer m = (HasUConfig m, HasDBPool m, MonadLoggerIO m, MonadUnliftIO m, MonadMask m, MonadCatch m)

app :: (HasServer m) => m ()
app = do
  sconfig <- getSConfig
  uconfig <- askM @UConfig
  dynuconfig <- askM @DynUConfig
  jobCon <- createJobCon

  pool :: Pool PS.SqlBackend <- askM
  --   -- Adding some configurations. 'Cookie' requires, in addition to
  --   -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = SA.defaultJWTSettings sconfig.cookieKey
      -- We don't need XSRF with SameSite set
      cookieCfg = (def @SA.CookieSettings){cookieSameSite = SA.SameSiteLax, cookieXsrfSetting = Nothing}
      cfg = cookieCfg :. jwtCfg :. S.EmptyContext
      settings =
        W.defaultSettings
          & W.setOnException (\_ e -> TIO.hPutStrLn stderr $ show e)
          & W.setPort uconfig.port
          & W.setHost (fromString $ T.unpack uconfig.host)
      webServer =
        liftIO $
          W.runSettings settings $
            W.logStdoutDev $ -- FIXME: remove logStdoutDev -- FIXME: remove logStdoutDev -- FIXME: remove logStdoutDev -- FIXME: remove logStdoutDev -- FIXME: remove logStdoutDev -- FIXME: remove logStdoutDev
            -- FIXME: remove logStdoutDev
              W.timeout uconfig.timeout $
                S.serveWithContextT
                  (Proxy @Api)
                  cfg
                  ( runStderrLoggingT
                      . flip runReaderT uconfig
                      . flip runReaderT dynuconfig
                      . flip runReaderT jobCon
                      . flip runReaderT pool
                      . unRHandler
                  )
                  (server def jwtCfg)
  concurrently_ webServer (runReaderT (jobRunner 50) jobCon)

runSomeApp :: [Char] -> (forall m. (HasServer m) => m ()) -> IO ()
runSomeApp uconfigPath m = do
  uconfig <- getUConfig uconfigPath
  dynuconfig <- getDynUConfig uconfig
  -- FIXME: fix user in connection string
  let conStr = T.encodeUtf8 uconfig.dbConfig.connectionString
  runCTStderrLoggingT $
    PS.withPostgresqlPool conStr uconfig.dbConfig.connections \pool ->
      flip runReaderT uconfig $
        flip runReaderT dynuconfig $
          flip runReaderT pool do
            doMigration
            m
