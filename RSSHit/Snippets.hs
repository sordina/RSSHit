module Snippets where

import Settings
import Settings.StaticFiles ()
import Yesod.Static ()
import Yesod.Widget

name, whatis, rssitem, loginItems, footer :: GWidget sub master ()

name       = $(widgetFile "name")
whatis     = $(widgetFile "whatis")
rssitem    = $(widgetFile "rssitem")
loginItems = $(widgetFile "login_items")
footer     = $(widgetFile "login_items")
