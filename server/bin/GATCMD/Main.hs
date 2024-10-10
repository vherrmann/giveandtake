module Main where

import Database.Persist qualified as P
import GiveAndTake.App (runSomeApp)
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Options.Applicative hiding (command)
import Options.Applicative qualified as O

-- sendNotif :: IO ()
-- sendNotif = do
--     text <- T.getLine
--     prio <- T.getLine
--     todo

-- notifCmd :: Parser NewNotif
-- notifCmd =
--     NewNotif
--         <$> strOption (long "title" <> short 't' <> help "Title of the notification")
--         <*> (NotifWelcome <$> strOption (long "" <> short 'c' <> help "Content of the notification"))
--         <*> option auto (long "prio" <> short 'p' <> help "Priority of the notification")

--   hsubparser $
--       cmd "notify" (ipi $ sendNotif)
-- where
--   ipi a = info (pure a) idm
--   cmd = command

data Command = NewAuthCode AuthCodeType

data Config = Config
    { uconfigPath :: [Char]
    , command :: Command
    }

newAuthCodeP :: O.Parser AuthCodeType
newAuthCodeP = option auto (long "type" <> short 't' <> help "Type of the auth code")

uconfigPathP :: O.Parser [Char]
uconfigPathP = strOption (long "config" <> short 'c' <> help "Path to config file")

cmdApp :: forall m. (HasUConfig m, HasDBPool m, MonadLoggerIO m) => Command -> m ()
cmdApp (NewAuthCode codeType) = do
    createdAt <- getUTCTime
    secret <- randomUrlToken
    runDB $ P.insert_ AuthCode{authType = codeType, secret, used = False, createdAt}
    uconfig <- askM @UConfig
    let url :: Text = case codeType of
            ACTSignup -> authUrl uconfig ["signup"]
    putStrLn @Text [fmt|Url: {url}?secret={secret}|]

main :: IO ()
main = do
    config <- liftIO $ execParser (info (opts <**> helper) idm)
    runSomeApp config.uconfigPath (cmdApp config.command)
  where
    opts :: O.Parser Config
    opts = do
        uconfigPath <- uconfigPathP
        command <- NewAuthCode <$> hsubparser (O.command "newAuthCode" (info newAuthCodeP idm))
        pure $ Config{..}
