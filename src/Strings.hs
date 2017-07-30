module Strings where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

type UTF8BString = UTF8.ByteString

(@+) :: UTF8BString -> UTF8BString -> UTF8BString
(@+) = B.append

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

stringToUtf8 :: String -> UTF8BString
stringToUtf8 = UTF8.fromString

utf8ToString :: UTF8BString -> String
utf8ToString = UTF8.toString
