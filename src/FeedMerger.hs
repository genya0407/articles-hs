module FeedMerger
  ( mergeFeedsIntoEntries,
    GenericEntry (..),
  )
where

import Control.DeepSeq
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, strip, unpack)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM, rfc822DateFormat)
import GHC.Generics
import Maybes (firstJusts)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Types as T
import qualified Text.RSS.Syntax as RSS

data GenericEntry = GenericEntry
  { title :: Text,
    entryUrl :: Text,
    abstract :: Text,
    iconUrl :: Maybe Text,
    publishedAt :: UTCTime
  }
  deriving (Show, Generic, NFData)

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
            abstract = fromMaybe ("" :: Text) $ (firstJusts [fmap textContentToText (Atom.entrySummary e), fmap entryContentToText (Atom.entryContent e)] :: Maybe Text),
            iconUrl = Atom.linkHref <$> find (\l -> Atom.linkRel l == Just (Left "enclosure")) (Atom.entryLinks e),
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