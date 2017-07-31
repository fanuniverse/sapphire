module TestUtils where

import qualified Database.Redis as Redis

redis :: IO Redis.Connection
redis = Redis.checkedConnect $ Redis.defaultConnectInfo

flushdb :: IO ()
flushdb = redis >>= (flip Redis.runRedis) Redis.flushdb >> return ()
