{-# LANGUAGE OverloadedStrings #-}
{- Standalone parser implementation -}
module NineMorris.Parsers.Config where

--import NineMorris.Globals as Globals
import Data.Attoparsec.Text
import Data.Text (unpack)
import Data.List as List
import Control.Applicative
import Control.Exception

type Store = (String,String)
data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)

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
  key <- takeWhile1 (not.(\c -> c=='#' || c=='=' || isHorizontalSpace c ))
  delimParser
  value <- takeWhile1 skipRestOfLine
  return $ ((unpack $ key),(unpack $ value))
  
createConfig :: (Either String [Store]) -> Config
createConfig (Right store) = Config {
                               hostname = getValue "hostname" store,
                               port     = read $ getValue "port" store,
                               gamekind = getValue "hostname" store
                             }
  where
    getValue key list = case List.lookup key list of
                             (Just a) -> a
                             Nothing  -> throw $ G.ConfigNotValid (key++" missing")
createConfig (Left _) = throw $ G.ConfigNotValid "parse error"

main :: IO ()
main = do
  print $ createConfig $ parseOnly lineParser "hostname=Host\n#test\nport=345 #test\r\ngamekind=test\n#comment\nkey=value\n"
