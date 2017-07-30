module Insertion (DataModifier, addTag, removeTag) where

import Strings (UTF8BString, (@+), stringToUtf8)

import Database.Redis (Connection, runRedis, zincrby)

import Control.Monad (forM_)
import Data.List (inits)
import Data.Set (Set)
import qualified Data.Set as Set

type DataModifier = String -> IO ()

prefixes :: String -> Set UTF8BString
prefixes = Set.map stringToUtf8 . Set.fromList . prefixList
  where
    prefixList = filter ((> 1) . length) . (inits =<<) . words

addTag :: Connection -> DataModifier
addTag redis tag = changeScore redis tag 1

removeTag :: Connection -> DataModifier
removeTag redis tag = changeScore redis tag (-1)

changeScore :: Connection -> String -> Integer -> IO ()
changeScore redis tag changeBy = runRedis redis $ forM_ (prefixes tag) update
  where
    update prefix = zincrby ("search:" @+ prefix) changeBy (stringToUtf8 tag)
