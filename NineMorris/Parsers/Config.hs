{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.Parsers.Config
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module parses a config file to a config record
-- this parser is capable of comments beginning with a hash
-- these comments can either be in a single line or inline
-- spaces at any position except in key or value names are skipped.
-- the delimiter between key and value must be the equal character,
-- might be lead and followed by many spaces.
-- LIMITATIONS:
-- No space or comment chars in key or value,
-- no quotes or escape characters to deal with this cases are possible
-- see the example config file include in the package
-----------------------------------------------------------------------------
module NineMorris.Parsers.Config (getConfig) where

import qualified NineMorris.Globals as G
import Data.Attoparsec.Text
import Data.Text (Text, pack, unpack, append)
import Data.List as List
import Control.Applicative
import Control.Exception

-- | key value pair
type Store = (Text,Text)

-- | skips comments and parses a single key value pair
productParser :: Parser Store
productParser =
     commentParser *> productParser                         -- parse comment line
 <|> parseKeyValue

-- | parses all lines and deals with a comment as last line
lineParser :: Parser [Store]
lineParser = many $ productParser <* skipSpace <* (commentParser <|> return ()) <* (endOfLine <|> return ()) --endofline or not (eg endoffile)

delimParser :: Parser ()
delimParser = skipSpace *> (string "=") *> skipSpace

commentParser :: Parser ()
commentParser = skipSpace *> ((char '#') *> skipWhile (not.isEndOfLine)) *> (endOfLine <|> return ())

skipRestOfLine :: Char -> Bool
skipRestOfLine c = (not $ (isEndOfLine) c || (isHorizontalSpace c || '#' == c))

-- | parses a single key value pair that is not a comment
parseKeyValue :: Parser Store
parseKeyValue = do
  skipSpace
  key <- takeWhile1 (not.(\c -> c=='#' || c=='=' || isHorizontalSpace c ))
  delimParser
  value <- takeWhile1 skipRestOfLine
  return $ (key,value)
  
-- | creates a config record from the parser results
createConfig :: (Either String [Store]) -> G.Config
createConfig (Right store) = G.Config {
                               G.hostname = getValue "hostname",
                               G.port     = read $ unpack $ getValue "port",
                               G.gamekind = getValue "gamekind"
                             }
  where
    getValue :: Text -> Text
    getValue key = case List.lookup key store of
                             (Just a) -> a
                             Nothing  -> throw $ G.ConfigNotValid (key `append` " missing")
createConfig (Left _) = throw $ G.ConfigNotValid "parse error"

-- | parses a config file to a config record
getConfig :: String     -- ^ file path
          -> G.Config
getConfig content = createConfig $ parseOnly lineParser (pack $ content)

--main :: IO ()
--main = do
--  print $ createConfig $ parseOnly lineParser "hostname=Host\n#test\nport=345 #test\r\ngamekind=test\n#comment\nkey=value\n"
