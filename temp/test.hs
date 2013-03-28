{-# LANGUAGE OverloadedStrings #-}

import Text.Feed.Import      (parseFeedString)
import Text.Feed.Export      (xmlFeed)
import Text.Feed.Constructor
import Network.HTTP.Conduit  (simpleHttp)
import Data.ByteString.Lazy  (toStrict)
import Data.ByteString.Char8 (unpack)
import Text.XML.Light        (ppElement, add_attrs, Attr(..), attrKey, QName(..), qName, qURI, qPrefix, attrVal, Element)
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
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as L

main :: IO ()
main = do crap  <-  ppElement . addNamespaces . xmlFeed . createFeed . map convert . mergeRssItems
                <$> getFeeds [ "rss/3am.rss"           -- "http://www.3ammagazine.com/3am/feed/"
                             , "rss/haskellforall.rss" -- "http://www.haskellforall.com/feeds/posts/default?alt=rss"
                             ]

          scotty 3000 $ get "/" $ do
            header "Content-Type" "text/xml" -- New scotty version
            raw $ encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <> L.pack crap

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

addNamespaces :: Element -> Element
addNamespaces = add_attrs [ Attr {attrKey = QName {qName = "dc",      qURI = Nothing, qPrefix = Just "xmlns"}, attrVal = "http://purl.org/dc/elements/1.1/"}
                          , Attr {attrKey = QName {qName = "slash",   qURI = Nothing, qPrefix = Just "xmlns"}, attrVal = "http://purl.org/rss/1.0/modules/slash/"}
                          , Attr {attrKey = QName {qName = "content", qURI = Nothing, qPrefix = Just "xmlns"}, attrVal = "http://purl.org/rss/1.0/modules/content/"}
                          , Attr {attrKey = QName {qName = "wfw",     qURI = Nothing, qPrefix = Just "xmlns"}, attrVal = "http://www.wellformedweb.org/CommentAPI/"}
                          , Attr {attrKey = QName {qName = "atom",    qURI = Nothing, qPrefix = Just "xmlns"}, attrVal = "http://www.w3.org/2005/Atom"}
                          , Attr {attrKey = QName {qName = "thr",     qURI = Nothing, qPrefix = Just "xmlns"}, attrVal = "http://purl.org/syndication/thread/1.0"}
                          ]
