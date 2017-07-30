{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Retrieval where

import Strings (UTF8BString, (@+), utf8ToString, splitIntoTerms)

import Database.Redis (Connection, Aggregate(Max), runRedis,
  zinterstore, expire, zrevrangebyscoreWithscoresLimit)

import Data.ByteString (intercalate)

search :: Connection -> Integer -> UTF8BString -> IO [(String, Int)]
search redis count = go . splitIntoTerms
  where
    go [term] = runRedis redis $
      getTopTags ("search:" @+ term)
    go terms = runRedis redis $
      zinterstore intersectionKey termKeys Max
      >> expire intersectionKey 60
      >> getTopTags intersectionKey
      where
        intersectionKey = intercalate "|" terms
        termKeys = ("search:" @+) <$> terms
    getTopTags key = withScoreAboveZero key 0 count >>= \case
      Right tags -> return $ (\(tag, score) ->
        (utf8ToString tag, round score)) <$> tags
      Left _ -> return []
    withScoreAboveZero key = zrevrangebyscoreWithscoresLimit key (1 / 0) 1
