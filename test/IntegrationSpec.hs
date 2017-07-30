module IntegrationSpec where

import Test.Hspec
import Test.Hspec.Wai

import Server (app)
import Strings (stringToUtf8)
import TestUtils (redis, flushdb)

import Control.Monad (replicateM_)

main :: IO ()
main = hspec spec

spec :: Spec
spec = before_ flushdb $ with (app <$> redis) $ do
  describe "adding a new tag" $
    it "increments its score" $ do
      get "/search?q=tag" `shouldRespondWith` "[]"
      get "/update?add=[\"tag\"]" `shouldRespondWith` 200
      get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"

  describe "removing an existing tag" $ do
    it "decrements its score" $ do
      replicateM_ 2 $ get "/update?add=[\"tag\"]"
      get "/search?q=tag" `shouldRespondWith` "[[\"tag\",2]]"
      get "/update?remove=[\"tag\"]" `shouldRespondWith` 200
      get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"

    it "hides the tag once its score reaches 0" $ do
      replicateM_ 2 $ get "/update?add=[\"tag\"]"
      get "/search?q=tag" `shouldRespondWith` "[[\"tag\",2]]"
      replicateM_ 2 $ get "/update?remove=[\"tag\"]" `shouldRespondWith` 200
      get "/search?q=tag" `shouldRespondWith` "[]"

  describe "searching the default bucket" $ do
    it "returns the results ordered by score" $ do
      replicateM_ 2 $ get "/update?add=[\"a tag\"]"
      replicateM_ 5 $ get "/update?add=[\"second tag\"]"
      replicateM_ 3 $ get "/update?add=[\"third tag\"]"
      get "/search?q=tag" `shouldRespondWith`
        "[[\"second tag\",5],[\"third tag\",3],[\"a tag\",2]]"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"] }

    it "can search by multiple prefixes" $
      get "/update?add=[\"pearl\"\
        \,\"revengeful\"\
        \,\"terrifying renegade pearl\"\
        \,\"rebellious peridot\"\
        \,\"restrained sapphire\"\
        \,\"crying pear\"]" >>
      get "/search?q=re pe" `shouldRespondWith`
        "[[\"terrifying renegade pearl\",1]\
        \,[\"rebellious peridot\",1]]"

  describe "searching different buckets" $ do
    it "separates results by buckets" $
      get "/update?add=[\"artist: aqueousy\"\
        \,\"artist: aquatic trickster\"\
        \,\"artist: aquacultural being\"\
        \,\"aquamarine\"]" >>
      get "/search?q=aqua" `shouldRespondWith`
        "[[\"aquamarine\",1]]" >>
      get "/search?q=artist: aqua" `shouldRespondWith`
        "[[\"artist: aquatic trickster\",1]\
        \,[\"artist: aquacultural being\",1]]"

    it "can search by multiple prefixes inside a bucket" $
      get "/update?add=[\"fandom: steven universe\"\
        \,\"fandom: stationery unicorn\"\
        \,\"fandom: staple unpicker\"]" >>
      get "/search?q=st" `shouldRespondWith`
        "[]" >>
      get "/search?q=fandom: st uni" `shouldRespondWith`
        "[[\"fandom: steven universe\",1]\
        \,[\"fandom: stationery unicorn\",1]]"

  describe "encoding" $
    it "properly indexes unicode tags" $
      get (stringToUtf8 "/update?add=[\"юникод\",\"юникод юникод\"]") >>
      get (stringToUtf8 "/search?q=юни") `shouldRespondWith`
        "[[\"юникод юникод\",1]\
        \,[\"юникод\",1]]"
