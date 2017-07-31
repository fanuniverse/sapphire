module Main where

import Server (runServer)

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main :: IO ()
main = runServer =<< redisHost
  where
    redisHost = fromMaybe "localhost" <$> lookupEnv "REDIS_HOST"
