{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy as BLIO (readFile)
import Data.Maybe
import FeedBuilder (feedsIntoEntries)
import Test.HUnit
import Text.Feed.Import

testFeedsIntoEntries :: Test
testFeedsIntoEntries = TestCase $ do
  feedTexts <- forM ["test/fixtures/atom.xml", "test/fixtures/rss.xml"] BLIO.readFile
  let genericEntries = feedsIntoEntries feedTexts
  evaluate $ rnf genericEntries -- evaluate strictly
  assertEqual "expect entry count" (length genericEntries) 33

tests :: Test
tests = TestList [testFeedsIntoEntries]

main :: IO ()
main = do
  counts <- runTestTT tests
  return ()