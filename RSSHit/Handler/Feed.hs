{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Feed where

import Import

maybe404 :: Maybe x -> GHandler sub0 master0 x
maybe404 Nothing   = notFound
maybe404  (Just x) = return x

getFeedR :: Text -> Handler RepHtml
getFeedR uName = do

  u  <- runDB    $ getBy404   $ UniqueUser uName
  uF <- runDB    $ selectList [FeedUser ==. entityKey u] []

  liftIO $ print u
  liftIO $ print uF

  defaultLayout $ do
      aDomId <- lift newIdent
      setTitle "RssHit - Really Simple Shit"
      $(widgetFile "homepage")
