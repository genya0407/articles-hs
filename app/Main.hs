{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe
import Data.Ord
import Data.Sort
import Data.Text (Text, pack)
import Data.Text.IO as T (putStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO as TL (putStrLn)
import Data.Time.Clock
import Data.Time.Format
import FeedMerger
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    defaultHttpConfig,
    req,
    responseBody,
    runReq,
    useHttpsURI,
  )
import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.Feed.Import (parseFeedSource)
import Text.URI (mkURI)

fetchAllEntries :: [Text] -> IO [GenericEntry]
fetchAllEntries urls = do
  feeds <- forM urls $ \feedUrlText -> do
    feedUrl <- mkURI feedUrlText
    let Just (uri, option) = useHttpsURI feedUrl
    resp <- runReq defaultHttpConfig $ req GET uri NoReqBody bsResponse option
    let feedText = responseBody resp
    let Just feed = parseFeedSource $ decodeUtf8 $ fromStrict feedText
    return feed
  return $ FeedMerger.mergeFeedsIntoEntries feeds

myFeeds :: [Text]
myFeeds =
  [ "https://dawn.hateblo.jp/feed",
    "https://qiita.com/genya0407/feed",
    "https://genya0407.github.io/feed.xml"
  ]

buildFeed :: Text -> Text -> [GenericEntry] -> Feed
buildFeed feedUrl feedTitleText entries =
  let feedTitle = TextString feedTitleText
      atomFormatTime = pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Ez"))
      emptyFeed = nullFeed feedUrl feedTitle (atomFormatTime . maximum . map FeedMerger.publishedAt $ entries)
      atomEntries = flip map entries $ \entry ->
        let atomEntry =
              nullEntry
                (FeedMerger.entryUrl entry)
                (TextString . FeedMerger.title $ entry)
                (atomFormatTime . FeedMerger.publishedAt $ entry)
            atomIconUrl = (\url -> (nullLink url) {linkRel = Just (Left "enclosure")}) <$> FeedMerger.iconUrl entry
         in atomEntry
              { entryContent = Just . TextContent . FeedMerger.abstract $ entry,
                entryLinks = entryLinks atomEntry ++ maybeToList atomIconUrl
              }
   in emptyFeed {feedEntries = atomEntries}

main :: IO ()
main = do
  entries <- fetchAllEntries myFeeds
  let sortedEntries = sortOn (Down . FeedMerger.publishedAt) entries
      feed = buildFeed "https://articles.genya0407.net" "さんちゃのブログ" sortedEntries
  TL.putStrLn . fromJust $ textFeed feed
