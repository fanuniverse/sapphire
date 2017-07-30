module Insertion (DataModifier, addTag, removeTag) where

import Strings (UTF8BString, (@+),
  prefixesForTermsWithLength, splitIntoBucketAndTerms)

import Control.Monad (forM_)
import Database.Redis (Connection, runRedis, zincrby)

type DataModifier = UTF8BString -> IO ()

addTag :: Connection -> DataModifier
addTag redis tag = changeScore redis tag 1

removeTag :: Connection -> DataModifier
removeTag redis tag = changeScore redis tag (-1)

changeScore :: Connection -> UTF8BString -> Integer -> IO ()
changeScore redis tag incrementBy = runRedis redis $ do
  forM_ prefixes $ \prefix ->
    zincrby (bucket @+ prefix) incrementBy tag
  where
    prefixes = prefixesForTermsWithLength (>= 2) terms
    (bucket, terms) = splitIntoBucketAndTerms tag
