module Snippets where

import Settings
import Settings.StaticFiles
import Yesod.Static

name    = $(widgetFile "name")
whatis  = $(widgetFile "whatis")
rssitem = $(widgetFile "rssitem")
