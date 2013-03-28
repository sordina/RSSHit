{-| Format taken from http://www.faqs.org/rfcs/rfc822.html |-}

module RFC822 (parse) where

import Safe
import Data.Maybe
import Control.Applicative

parse :: String -> [String]
parse x = catMaybes [w 3, month <$> w 2, w 1]
  where
    w = atMay $ words x

month :: String -> String
month "Jan" = "01"
month "Feb" = "02"
month "Mar" = "03"
month "Apr" = "04"
month "May" = "05"
month "Jun" = "06"
month "Jul" = "07"
month "Aug" = "08"
month "Sep" = "09"
month "Oct" = "10"
month "Nov" = "11"
month "Dec" = "12"
month x     = error $ x ++ " is not a vailid month"
