module Bob (responseFor) where

import qualified Data.Char as C
import qualified Data.Text as T
import Data.Text (Text)

isEmpty :: Text -> Bool
isEmpty text = text == T.empty

isQuestion :: Text -> Bool
isQuestion text = T.last (T.strip text) == '?'

filterAlpha :: Text -> Text
filterAlpha text = T.filter C.isAlpha text

allUpper :: Text -> Bool
allUpper text
  | isEmpty text = False
  | otherwise    = T.all C.isUpper text

allNotAlphaNum :: Text -> Bool
allNotAlphaNum text = T.all (not.C.isAlphaNum) text

responseFor :: String -> String
responseFor xs 
  | isEmpty text                   = "Fine. Be that way!"
  | isQuestion text
    && allUpper (filterAlpha text) = "Calm down, I know what I'm doing!"
  | isQuestion text                = "Sure."
  | allNotAlphaNum text            = "Fine. Be that way!"
  | allUpper (filterAlpha text)    = "Whoa, chill out!"
  | otherwise                      = "Whatever."
  where text = T.strip $ T.pack xs

