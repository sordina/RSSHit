{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Language.Haskell.TH ( Exp(..) )

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "RssHit - Really Simple Shit"
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Home")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
