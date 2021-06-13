module FeedBuilder (feedsIntoEntries, buildFeed, GenericEntry (..)) where

import Control.DeepSeq
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Maybe
import Data.Ord
import Data.Sort
import Data.Text (Text, pack, strip, unpack)
import Data.Text.IO as T (putStrLn)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.IO as TL (putStrLn)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Format
import GHC.Generics
import Maybes (firstJusts)
import Text.Atom.Feed
import qualified Text.Atom.Feed as Atom
import Text.Atom.Feed.Export
import Text.Feed.Import (parseFeedSource)
import qualified Text.Feed.Types as T
import qualified Text.RSS.Syntax as RSS

buildFeed :: Text -> Text -> [GenericEntry] -> Feed
buildFeed feedUrl feedTitleText entries =
  let feedTitle = TextString feedTitleText
      atomFormatTime = pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Ez"))
      emptyFeed = nullFeed feedUrl feedTitle (atomFormatTime . maximum . map publishedAt $ entries)
      atomEntries = flip map entries $ \entry ->
        let atomEntry =
              nullEntry
                (entryUrl entry)
                (TextString . title $ entry)
                (atomFormatTime . publishedAt $ entry)
            atomIconUrl = (\url -> (nullLink url) {linkRel = Just (Right "enclosure")}) <$> iconUrl entry
         in atomEntry
              { entryContent = Just . TextContent . abstract $ entry,
                entryLinks = entryLinks atomEntry ++ maybeToList atomIconUrl
              }
   in emptyFeed {feedEntries = atomEntries}

data GenericEntry = GenericEntry
  { title :: Text,
    entryUrl :: Text,
    abstract :: Text,
    iconUrl :: Maybe Text,
    publishedAt :: UTCTime
  }
  deriving (Show, Generic, NFData)

feedsIntoEntries :: [BL.ByteString] -> [GenericEntry]
feedsIntoEntries = sortOn (Down . publishedAt) . mergeFeedsIntoEntries . mapMaybe parseFeedSource

mergeFeedsIntoEntries :: [T.Feed] -> [GenericEntry]
mergeFeedsIntoEntries [] = []
mergeFeedsIntoEntries (f : fs) = case f of
  T.AtomFeed atom -> extractEntriesAtom atom ++ mergeFeedsIntoEntries fs
  T.RSSFeed rss -> extractEntriesRss rss ++ mergeFeedsIntoEntries fs

extractEntriesAtom :: Atom.Feed -> [GenericEntry]
extractEntriesAtom feed =
  let entries = Atom.feedEntries feed
      entryToEntry e =
        GenericEntry
          { title = textContentToText $ Atom.entryTitle e,
            entryUrl = (Atom.linkHref . head . Atom.entryLinks) e,
            abstract = fromMaybe ("" :: Text) (firstJusts [fmap textContentToText (Atom.entrySummary e), fmap entryContentToText (Atom.entryContent e)] :: Maybe Text),
            iconUrl = Atom.linkHref <$> find (\l -> Atom.linkRel l == Just (Right "enclosure")) (Atom.entryLinks e),
            publishedAt = fromJust (parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Ez")) (unpack $ Atom.entryUpdated e) :: Maybe UTCTime)
          }
      entryContentToText (Atom.TextContent t) = t
      entryContentToText (Atom.HTMLContent t) = t
      textContentToText (Atom.TextString t) = t
      textContentToText (Atom.HTMLString t) = t
   in map entryToEntry entries

extractEntriesRss rss =
  let channel = RSS.rssChannel rss
      items = RSS.rssItems channel
      itemToEntry item =
        GenericEntry
          { title = strip . fromJust . RSS.rssItemTitle $ item,
            entryUrl = strip . fromJust . RSS.rssItemLink $ item,
            abstract = strip . fromJust . RSS.rssItemDescription $ item,
            iconUrl = strip . RSS.rssEnclosureURL <$> RSS.rssItemEnclosure item,
            publishedAt = fromJust (RSS.rssItemPubDate item >>= parseTimeM True defaultTimeLocale rfc822DateFormat . unpack :: Maybe UTCTime)
          }
   in map itemToEntry items
