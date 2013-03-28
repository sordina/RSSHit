-- | TODO: Sort some of these convertions out more completely
--
module Conversion (convert) where

import Text.Feed.Types
import Text.Feed.Translate
import Data.Maybe
import Control.Applicative
import Safe

import qualified Text.Atom.Feed   as AtomModule
import qualified Text.RSS.Syntax  as RSSModule
import qualified Text.RSS1.Syntax as RSS1Module

-- | Convert any feed type to Atom
convert :: Item -> Item
convert i@(AtomItem _x) = i
convert   (RSSItem   x) = AtomItem $ rssToAtom  x
convert   (RSS1Item  x) = AtomItem $ rss1ToAtom x
convert i@(XMLItem  _x) = translateItemTo AtomKind i

rssToAtom :: RSSModule.RSSItem -> AtomModule.Entry
rssToAtom (RSSModule.RSSItem   rTitle              -- :: Maybe String
                               rLink               -- :: Maybe URLString
                               rDescription        -- :: Maybe String     -- ^if not present, the title is. (per spec, at least.)
                               rAuthor             -- :: Maybe String
                               rCategories         -- :: [RSSCategory]
                               _rComments          -- :: Maybe URLString
                               _rEnclosure         -- :: Maybe RSSEnclosure
                               rGuid               -- :: Maybe RSSGuid
                               rPubDate            -- :: Maybe DateString
                               rSource             -- :: Maybe RSSSource
                               rAttrs              -- :: [XML.Attr]
                               rOther              -- :: [XML.Element]

          ) = AtomModule.Entry eId                 -- :: String
                               eTitle              -- :: TextContent
                               eUpdated            -- :: Date  -- Just a string
                               eAuthors            -- :: [Person]
                               eCategories         -- :: [Category]
                               eContent            -- :: Maybe EntryContent
                               eContributor        -- :: [Person]
                               eLinks              -- :: [Link]
                               ePublished          -- :: Maybe Date
                               eRights             -- :: Maybe TextContent
                               eSource             -- :: Maybe Source
                               eSummary            -- :: Maybe TextContent
                               eInReplyTo          -- :: Maybe InReplyTo
                               eInReplyTotal       -- :: Maybe InReplyTotal
                               eAttrs              -- :: [XML.Attr]
                               eOther              -- :: [XML.Element]

   where eId           = fromMaybe "NoId" $ RSSModule.rssGuidValue <$> rGuid
         eTitle        = AtomModule.HTMLString $ fromMaybe "NoTitle" rTitle
         eUpdated      = fromMaybe "NoDate" rPubDate
         eAuthors      = map (\a -> AtomModule.Person a Nothing Nothing []) (maybeToList rAuthor)
         eCategories   = map (\a -> AtomModule.Category (RSSModule.rssCategoryValue a) Nothing Nothing []) rCategories
         eContent      = AtomModule.HTMLContent <$> rDescription
         eContributor  = []
         eLinks        = (\l -> AtomModule.Link l Nothing Nothing Nothing Nothing Nothing [] []) <$> maybeToList rLink
         ePublished    = rPubDate
         eRights       = Nothing
         eSource       = (\s -> AtomModule.Source [] [] Nothing Nothing Nothing
                                                  [AtomModule.Link (RSSModule.rssSourceURL s) Nothing Nothing Nothing Nothing Nothing [] []]
                                                  Nothing Nothing Nothing
                                                  (Just $ AtomModule.HTMLString $ RSSModule.rssSourceTitle s)
                                                  Nothing []
                                                  ) <$> rSource
         eSummary      = Nothing
         eInReplyTo    = Nothing
         eInReplyTotal = Nothing
         eAttrs        = rAttrs
         eOther        = rOther

rss1ToAtom :: RSS1Module.Item -> AtomModule.Entry
rss1ToAtom (RSS1Module.Item     iURI          -- :: URIString
                                iTitle        -- :: TextString
                                iLink         -- :: URIString
                                iDesc         -- :: Maybe TextString
                                _iDC          -- :: [DCItem]
                                iTopics       -- :: [URIString]
                                iContent      -- :: [ContentInfo]
                                iOther        -- :: [XML.Element]
                                iAttrs        -- :: [XML.Attr]

           ) = AtomModule.Entry eId           -- :: String
                                eTitle        -- :: TextContent
                                eUpdated      -- :: Date
                                eAuthors      -- :: [Person]
                                eCategories   -- :: [Category]
                                eContent      -- :: Maybe EntryContent
                                eContributor  -- :: [Person]
                                eLinks        -- :: [Link]
                                ePublished    -- :: Maybe Date
                                eRights       -- :: Maybe TextContent
                                eSource       -- :: Maybe Source
                                eSummary      -- :: Maybe TextContent
                                eInReplyTo    -- :: Maybe InReplyTo
                                eInReplyTotal -- :: Maybe InReplyTotal
                                eAttrs        -- :: [XML.Attr]
                                eOther        -- :: [XML.Element]

  where eId           = iURI
        eTitle        = AtomModule.HTMLString iTitle
        eUpdated      = "NoDate"
        eAuthors      = []
        eCategories   = (\t -> AtomModule.Category t Nothing Nothing []) <$> iTopics
        eContent      = AtomModule.HTMLContent <$> (headMay iContent >>= RSS1Module.contentValue)
        eContributor  = []
        eLinks        = [AtomModule.Link iLink Nothing Nothing Nothing Nothing Nothing [] []]
        ePublished    = Nothing
        eRights       = Nothing
        eSource       = Nothing
        eSummary      = AtomModule.HTMLString <$> iDesc
        eInReplyTo    = Nothing
        eInReplyTotal = Nothing
        eAttrs        = iAttrs
        eOther        = iOther
