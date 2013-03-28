{-# LANGUAGE OverloadedStrings #-}

import Text.Feed.Import      (parseFeedString)
import Text.Feed.Export      (xmlFeed)
import Text.Feed.Constructor
import Network.HTTP.Conduit  (simpleHttp)
import Data.ByteString.Lazy  (toStrict)
import Data.ByteString.Char8 (unpack)
import Text.XML.Light        (ppElement)
import RFC822                (parse)
import Conversion            (convert)
import Text.Feed.Types
import Text.Feed.Query
import Prelude
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Ord
import Data.Monoid
import Conc
import Web.Scotty
import qualified Data.Text.Lazy as L
-- import Text.Groom

main :: IO ()
main = do crap  <-  ppElement . xmlFeed . createFeed . map convert . mergeRssItems
                <$> getFeeds [ "3am.rss"           -- "http://www.3ammagazine.com/3am/feed/"
                             , "haskellforall.rss" -- "http://www.haskellforall.com/feeds/posts/default?alt=rss"
                             ]

          scotty 3000 $ get "/" $ do
            header "Content-Type" "application/atom+xml" -- Gets overwritten by scotty...
            text $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <> L.pack crap

mergeRssItems :: [Feed] -> [Item]
mergeRssItems = reverse . sortBy (comparing (fmap parse . getItemDate)) . concatMap getFeedItems

headS :: Int -> String -> String
headS n = unlines . take n . lines

getRssHttp :: String -> IO (Maybe Feed)
getRssHttp f = parseFeedString . unpack . toStrict <$> simpleHttp f

getRssFile :: FilePath -> IO (Maybe Feed)
getRssFile f = parseFeedString <$> readFile f

getFeeds :: [String] -> IO [Feed]
getFeeds l = catMaybes <$> forkJoin getRssFile l

createFeed :: [Item] -> Feed
createFeed items = withFeedItems items (newFeed AtomKind)
