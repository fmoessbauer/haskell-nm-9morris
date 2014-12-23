{-# LANGUAGE OverloadedStrings #-}
{- Standalone parser implementation -}
module NineMorris.Parsers.Config (getConfig) where

import qualified NineMorris.Globals as G
import Data.Attoparsec.Text
import Data.Text (pack, unpack)
import Data.List as List
import Control.Applicative
import Control.Exception

type Store = (String,String)

productParser :: Parser Store
productParser =
     commentParser *> productParser                         -- parse comment line
 <|> parseKeyValue

lineParser :: Parser [Store]
lineParser = many $ productParser <* skipSpace <* (commentParser <|> return ()) <* (endOfLine <|> return ()) --endofline or not (eg endoffile)

delimParser :: Parser ()
delimParser = skipSpace *> (string "=") *> skipSpace

commentParser :: Parser ()
commentParser = skipSpace *> ((char '#') *> skipWhile (not.isEndOfLine)) *> (endOfLine <|> return ())

skipRestOfLine :: Char -> Bool
skipRestOfLine c = (not $ (isEndOfLine) c || (isHorizontalSpace c || '#' == c))

parseKeyValue :: Parser Store
parseKeyValue = do
  skipSpace
  key <- takeWhile1 (not.(\c -> c=='#' || c=='=' || isHorizontalSpace c ))
  delimParser
  value <- takeWhile1 skipRestOfLine
  return $ ((unpack $ key),(unpack $ value))
  
createConfig :: (Either String [Store]) -> G.Config
createConfig (Right store) = G.Config {
                               G.hostname = getValue "hostname",
                               G.port     = read $ getValue "port",
                               G.gamekind = getValue "gamekind"
                             }
  where
    getValue key = case List.lookup key store of
                             (Just a) -> a
                             Nothing  -> throw $ G.ConfigNotValid (key++" missing")
createConfig (Left _) = throw $ G.ConfigNotValid "parse error"

getConfig :: String -> G.Config
getConfig content = createConfig $ parseOnly lineParser (pack $ content)

--main :: IO ()
--main = do
--  print $ createConfig $ parseOnly lineParser "hostname=Host\n#test\nport=345 #test\r\ngamekind=test\n#comment\nkey=value\n"
