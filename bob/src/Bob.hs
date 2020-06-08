module Bob (responseFor) where

import qualified Data.Char as C
import qualified Data.Text as T
import Data.Text (Text)

isEmpty :: Text -> Bool
isEmpty text = text == T.empty

isQuestion :: Text -> Bool
isQuestion text
  | isEmpty text = False
  | otherwise    = T.last (T.strip text) == '?'

filterAlpha :: Text -> Text
filterAlpha = T.filter C.isAlpha

allUpper :: Text -> Bool
allUpper text
  | isEmpty text = False
  | otherwise    = T.all C.isUpper text

allNotAlphaNum :: Text -> Bool
allNotAlphaNum = T.all (not.C.isAlphaNum)

responseFor :: String -> String
responseFor xs 
  | isQuestion text                = if allUpper (filterAlpha text)
                                       then "Calm down, I know what I'm doing!"
                                       else "Sure."
  | allNotAlphaNum text            = "Fine. Be that way!"
  | allUpper (filterAlpha text)    = "Whoa, chill out!"
  | otherwise                      = "Whatever."
  where text = T.strip $ T.pack xs

