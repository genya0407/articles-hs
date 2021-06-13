{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.IO as TL (putStrLn)
import FeedBuilder (buildFeed, feedsIntoEntries)
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
import Text.URI (mkURI)
import Web.Spock
import Web.Spock.Action
import Web.Spock.Config

myFeeds :: [Text]
myFeeds =
  [ "https://dawn.hateblo.jp/feed",
    "https://qiita.com/genya0407/feed",
    "https://genya0407.github.io/feed.xml"
  ]

fetchAllFeedTexts :: [Text] -> IO [ByteString]
fetchAllFeedTexts urls = do
  forM urls $ \feedUrlText -> do
    feedUrl <- mkURI feedUrlText
    let Just (uri, option) = useHttpsURI feedUrl
    resp <- runReq defaultHttpConfig $ req R.GET uri NoReqBody bsResponse option
    return $ responseBody resp

fetchFeed = do
  feedTexts <- fetchAllFeedTexts myFeeds
  let entries = feedsIntoEntries $ map BL.fromStrict feedTexts
  let feed = buildFeed "https://articles.genya0407.net" "さんちゃのブログ" entries
  return . fromJust . textFeed $ feed

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock spockCfg app)

app = do
  get "feed.xml" $ do
    feed <- liftIO fetchFeed
    setHeader "Content-Type" "application/xml; charset=utf-8"
    bytes . encodeUtf8 . toStrict $ feed
