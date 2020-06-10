module Bob (responseFor) where

import qualified Data.Char as C
import qualified Data.Text as T
import Data.Text (Text)

isEmpty :: Text -> Bool
isEmpty text = text == T.empty

isQuestion :: Text -> Bool
isQuestion text = T.last (T.strip text) == '?'

isYelling :: Text -> Bool
isYelling text = allUpper (filterAlpha text)

filterAlpha :: Text -> Text
filterAlpha = T.filter C.isAlpha

allUpper :: Text -> Bool
allUpper text
  | isEmpty text = False
  | otherwise    = T.all C.isUpper text

isAllUpperQuestion :: Text -> Bool
isAllUpperQuestion text = isQuestion text && isYelling text

isSayingNothing :: Text -> Bool
isSayingNothing text
  | isEmpty text = True
  | otherwise    = (not.isQuestion) text && T.all (not.C.isAlphaNum) text

responseFor :: String -> String
responseFor xs 
  | isSayingNothing text    = "Fine. Be that way!"
  | isAllUpperQuestion text = "Calm down, I know what I'm doing!"
  | isQuestion text         = "Sure."
  | isYelling text          = "Whoa, chill out!"
  | otherwise               = "Whatever."
  where text = T.strip $ T.pack xs

