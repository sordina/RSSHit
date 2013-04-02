{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Feed where

import Import
import Language.Haskell.TH ( Exp(..) )
import Yesod.Auth
import Yesod.Persist
import Data.Text

maybe404 Nothing   = notFound
maybe404  (Just x) = return x

getFeedR :: Text -> Handler RepHtml
getFeedR uName = do

  u  <- runDB    $ getBy404      $ UniqueUser uName
  let uI = entityKey u
  uF <- runDB    $ selectList    [FeedUser ==. uI] []

  liftIO $ print "User:"
  liftIO $ print u
  liftIO $ print uF

  defaultLayout $ do
      aDomId <- lift newIdent
      setTitle "RssHit - Really Simple Shit"
      $(widgetFile "homepage")
