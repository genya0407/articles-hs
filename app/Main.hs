{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.Trans
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text as T (Text, take)
import Data.Text.Encoding
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.IO as TL (putStrLn)
import Data.Time.Clock
import FeedBuilder
import GHC.Generics
import Maybes
import Network.HTTP.Req as R
  ( GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    defaultHttpConfig,
    req,
    responseBody,
    runReq,
    useHttpsURI,
  )
import Text.Atom.Feed.Export
import Text.EDE as EDE
import Text.URI (mkURI)
import Web.Spock
import Web.Spock.Action
import Web.Spock.Config

data Source = Source {sourceTitle :: Text, sourceUrl :: Text, sourceFeedUrl :: Text} deriving (Generic)

mySources :: [Source]
mySources =
  [ Source "さんちゃのブログ" "https://dawn.hateblo.jp/" "https://dawn.hateblo.jp/feed",
    Source "さんちゃのブログ 2nd" "https://genya0407.github.io/" "https://genya0407.github.io/feed.xml",
    Source "genya0407 - Qiita" "https://qiita.com/genya0407/" "https://qiita.com/genya0407/feed"
  ]

defaultIconURL :: Text
defaultIconURL = "/static/images/default.jpg"

myFeeds :: [Text]
myFeeds = map sourceFeedUrl mySources

data ViewEntry = ViewEntry
  { title :: Text,
    entryUrl :: Text,
    abstract :: Text,
    iconUrl :: Text,
    publishedAt :: UTCTime
  }
  deriving (Show, Generic)

fromGeneric :: GenericEntry -> ViewEntry
fromGeneric genericEntry =
  ViewEntry
    { Main.title = FeedBuilder.title genericEntry,
      Main.entryUrl = FeedBuilder.entryUrl genericEntry,
      Main.abstract = T.take 200 $ FeedBuilder.abstract genericEntry,
      Main.iconUrl = FeedBuilder.iconUrl genericEntry `orElse` defaultIconURL,
      Main.publishedAt = FeedBuilder.publishedAt genericEntry
    }

instance ToJSON ViewEntry

instance ToJSON Source

fetchAllFeedTexts :: [Text] -> IO [ByteString]
fetchAllFeedTexts urls = do
  forM urls $ \feedUrlText -> do
    feedUrl <- mkURI feedUrlText
    let Just (uri, option) = useHttpsURI feedUrl
    resp <- runReq defaultHttpConfig $ req R.GET uri NoReqBody bsResponse option
    return $ responseBody resp

fetchEntries = do
  feedTexts <- fetchAllFeedTexts myFeeds
  let entries = feedsIntoEntries $ map BL.fromStrict feedTexts
  return entries

fetchFeed = do
  entries <- fetchEntries
  let feed = buildFeed "https://articles.genya0407.net" "さんちゃのブログ" entries
  return . fromJust . textFeed $ feed

main :: IO ()
main = do
  EDE.Success template <- parseFile "template/index.html.jinja2"
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock spockCfg (app template))

app template = do
  get "index.html" $ do
    entries <- liftIO fetchEntries
    let env = fromPairs ["entries" .= toJSON (map fromGeneric entries), "sources" .= toJSON mySources, "default_icon_url" .= defaultIconURL]
        EDE.Success result = render template env
    html . toStrict $ result

  get "feed.xml" $ do
    feed <- liftIO fetchFeed
    setHeader "Content-Type" "application/xml; charset=utf-8"
    bytes . encodeUtf8 . toStrict $ feed
