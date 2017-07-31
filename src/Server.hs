module Server (runServer, app) where

import Retrieval (search)
import Insertion (addTag, removeTag)
import Strings (readUtf8List)

import qualified Database.Redis as Redis

import Control.Monad (join, forM_)
import Data.Maybe (fromMaybe)
import Data.Aeson (encode)

import Network.Wai (Application, Request, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.HTTP.Types (ok200, badRequest400)
import Network.HTTP.Types.Header (hContentType)

runServer :: String -> IO ()
runServer redisHost = run 3030 . app =<< redisConnection
  where
    redisConnection = Redis.checkedConnect Redis.defaultConnectInfo
      { Redis.connectHost = redisHost }

app :: Redis.Connection -> Application
app redis request respond = respond =<<
  case pathInfo request of
    "update":_ ->
      update redis request >> return (responseLBS ok200 [] "")
    "search":_ ->
      case (param "q") of
        Just query -> search redis 10 query >>= \results ->
          return (responseLBS ok200 [jsonContentType] (encode results))
        Nothing ->
          return (responseLBS badRequest400 [] "")
    _ ->
      return (responseLBS badRequest400 [] "")
  where
    param name = join $ lookup name (queryString request)
    jsonContentType = (hContentType, "application/json;charset=utf-8")

update :: Redis.Connection -> Request -> IO ()
update redis request = go =<< parseRequestBody lbsBackEnd request
  where
    go (params, _) =
      forM_ (listParam "add") (addTag redis) >>
      forM_ (listParam "remove") (removeTag redis)
      where
        listParam name = fromMaybe [] (readUtf8List <$> (lookup name params))
