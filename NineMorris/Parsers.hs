{-# LANGUAGE OverloadedStrings #-}
{- Standalone parser implementation -}
module NineMorris.Parsers where

--import NineMorris.Global as Global
import Data.Attoparsec.Text
import Data.Text
import Control.Applicative

data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)
data Product = Hostname String | Port Int | Gamekind String | Undef String String deriving (Show)

productParser :: Parser Product
productParser =
     commentParser *> productParser                         -- parse comment line
 <|> ((string "hostname") *> delimParser *> parseHostname)  -- parse hostname
 <|> ((string "port")     *> delimParser *> parsePort)      -- parse port
 <|> ((string "gamekind") *> delimParser *> parseGamekind)  -- parse gamekind
 <|> parseKeyValue

lineParser :: Parser [Product]
lineParser = many $ productParser <* skipSpace <* (commentParser <|> return ()) <* (endOfLine <|> return ()) --endofline or not (eg endoffile)

delimParser :: Parser ()
delimParser = skipSpace *> (string "=") *> skipSpace

commentParser :: Parser ()
commentParser = skipSpace *> ((char '#') *> skipWhile (not.isEndOfLine)) *> (endOfLine <|> return ())

parseHostname :: Parser Product
parseHostname = do
    str <- (takeWhile1 skipRestOfLine)
    return $ Hostname (unpack $ str)

parsePort :: Parser Product
parsePort = do
    port <- decimal
    return $ Port port

parseGamekind :: Parser Product
parseGamekind = do
    kind <- takeWhile1 skipRestOfLine
    return $ Gamekind (unpack $ kind)

skipRestOfLine :: Char -> Bool
skipRestOfLine c = (not $ (isEndOfLine) c || (isHorizontalSpace c || '#' == c))

parseKeyValue :: Parser Product
parseKeyValue = do
  key <- takeWhile1 (not.(\c -> c=='#' || c=='=' || isHorizontalSpace c ))
  delimParser
  value <- takeWhile1 skipRestOfLine
  return $ Undef (unpack $ key) (unpack $ value)

main :: IO ()
main = do
  print $ parseOnly lineParser "hostname=Host\n#test\nport=345 #test\r\ngamekind=test\n#comment\nkey=value\n"