module Main (main) where

import GiveAndTake.App
import GiveAndTake.Prelude
import System.Environment (getArgs)

main :: IO ()
main = do
    uconfigPath : _ <- liftIO getArgs -- FIXME: use optparse-applicative
    runSomeApp uconfigPath app
