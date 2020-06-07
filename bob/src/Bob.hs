module Bob (responseFor) where

import qualified Data.Char as C
import qualified Data.Text as T
import Data.Text (Text)

isEmpty :: Text -> Bool
isEmpty text
  | text == T.empty = True
  | otherwise       = False

isQuestion :: Text -> Bool
isQuestion text
  | T.last (T.strip text) == '?' = True
  | otherwise                    = False

filterAlpha :: Text -> Text
filterAlpha text = T.filter C.isAlpha text

isAllUpper :: Text -> Bool
isAllUpper text
  | isEmpty text         = False
  | T.all C.isUpper text = True
  | otherwise            = False

responseFor :: String -> String
responseFor xs 
  | isEmpty text                     = "Fine. Be that way!"
  | isQuestion text
    && isAllUpper (filterAlpha text) = "Calm down, I know what I'm doing!"
  | isQuestion text                  = "Sure."
  | T.all (not.C.isAlphaNum) text    = "Fine. Be that way!"
  | isAllUpper (filterAlpha text)    = "Whoa, chill out!"
  | otherwise                        = "Whatever."
  where text = T.strip $ T.pack xs

