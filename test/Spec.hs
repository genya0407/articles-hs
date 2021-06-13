import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Text.IO as TIO
import FeedMerger (mergeFeedsIntoEntries)
import Test.HUnit
import Text.Feed.Import

testMergeFeedsIntoEntries :: Test
testMergeFeedsIntoEntries = TestCase $ do
  feeds <- forM ["test/fixtures/atom.xml", "test/fixtures/rss.xml"] (fmap fromJust . parseFeedFromFile)
  let genericEntries = mergeFeedsIntoEntries feeds
  evaluate $ rnf genericEntries -- evaluate strictly
  assertEqual "expect entry count" (length genericEntries) 33

tests :: Test
tests = TestList [testMergeFeedsIntoEntries]

main :: IO ()
main = do
  counts <- runTestTT tests
  return ()