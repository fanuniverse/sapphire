module Strings where

import Data.List (inits)
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

type UTF8BString = UTF8.ByteString

(@+) :: UTF8BString -> UTF8BString -> UTF8BString
(@+) = B.append

stringToUtf8 :: String -> UTF8BString
stringToUtf8 = UTF8.fromString

utf8ToString :: UTF8BString -> String
utf8ToString = UTF8.toString

readUtf8List :: UTF8BString -> [UTF8BString]
readUtf8List = (stringToUtf8 <$>) . read . utf8ToString

prefixesForTermsWithLength :: (Int -> Bool) -> [UTF8BString] -> Set UTF8BString
prefixesForTermsWithLength lenP = Set.map stringToUtf8 . Set.fromList . go
  where
    go = filter (lenP . length) . prefixes
    prefixes terms = inits =<< (utf8ToString <$> terms)

splitIntoBucketAndTerms :: UTF8BString -> (UTF8BString, [UTF8BString])
splitIntoBucketAndTerms = go . splitIntoTerms
  where
    go ("artist:" : restOfTerms) =
      ("artist_bucket:", restOfTerms)
    go ("fandom:" : restOfTerms) =
      ("fandom_bucket:", restOfTerms)
    go terms =
      ("default_bucket:", terms)

{-|
  A term is a single word (without spaces).

  The implementation is based on utf8-bytestring's `lines`; see
  https://hackage.haskell.org/package/utf8-string-1.0.1.1/docs/Data-ByteString-UTF8.html
-}
splitIntoTerms :: UTF8BString -> [UTF8BString]
splitIntoTerms str | B.null str = []
splitIntoTerms str =
  case B.elemIndex space str of
    Just x ->
      let (xs,ys) = B.splitAt x str
      in xs : splitIntoTerms (B.tail ys)
    Nothing ->
      [str]
  where
    space = 32
