{-# LANGUAGE LambdaCase #-}

module Server (runServer, app) where

import Retrieval (search)
import Insertion (addTag, removeTag)
import Strings (readUtf8List)

import qualified Database.Redis as Redis

import Control.Monad (join, forM_)
import Data.Maybe (fromMaybe)
import Data.Aeson (encode)

import Network.Wai (Application, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200, badRequest400)
import Network.HTTP.Types.Header (hContentType)

runServer :: IO ()
runServer = run 3030 . app =<< Redis.checkedConnect Redis.defaultConnectInfo

app :: Redis.Connection -> Application
app redis request respond = respond =<<
  case pathInfo request of
    "update":_ -> do
      forM_ (listParam "add") (addTag redis)
      forM_ (listParam "remove") (removeTag redis)
      return $ responseLBS ok200 [] ""
    "search":_ -> do
      case (param "q") of
        Just query -> search redis 10 query >>= \results ->
          return $ responseLBS ok200 jsonContentType (encode results)
        Nothing ->
          return $ responseLBS badRequest400 [] ""
    _ ->
      return $ responseLBS badRequest400 [] ""
  where
    listParam name = fromMaybe [] (readUtf8List <$> param name)
    param name = join $ lookup name (queryString request)
    jsonContentType = [(hContentType, "application/json;charset=utf-8")]
