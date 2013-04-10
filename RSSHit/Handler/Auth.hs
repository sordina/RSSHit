{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Auth where

import Import

postPersonaLoginR :: Handler ()
postPersonaLoginR = do
  liftIO $ print ("Got Login Request" :: String)
  rb <- runRequestBody
  liftIO $ print rb
  return ()

postPersonaLogoutR :: Handler ()
postPersonaLogoutR = return ()

instance Show FileInfo where
  show fi = mconcat ["<fi ", show (fileName fi), " ", show (fileContentType fi), ">"]
