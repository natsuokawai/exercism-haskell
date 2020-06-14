module Bob (responseFor) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Char as C
import qualified Data.Text as T
import Data.Text (Text)

isAllUpper :: Text -> Bool
isAllUpper text
  | text == T.empty = False
  | otherwise       = T.all C.isUpper text

responseFor :: Text -> String
responseFor text 
  | isQuestion && isYelling = "Calm down, I know what I'm doing!"
  | isQuestion              = "Sure."
  | isYelling               = "Whoa, chill out!"
  | isSayingNothing         = "Fine. Be that way!"
  | otherwise               = "Whatever."
  where
    isSayingNothing = T.all (not.C.isAlphaNum) text
    isQuestion      = T.isSuffixOf (T.pack "?") (T.strip text)
    isYelling       = isAllUpper (T.filter C.isAlpha text)

