{-# LANGUAGE OverloadedStrings #-}
{- Standalone parser implementation -}
module NineMorris.Parsers where

--import NineMorris.Global as Global
import Data.Attoparsec.Text
import Data.Text
import Control.Applicative

data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)
data Product = Hostname String | Port Int | Gamekind String deriving (Show)

productParser :: Parser Product
productParser =
     commentParser
 <|> ((string "hostname") *> delimParser *> parseHostname)
 <|> ((string "port") *> delimParser *> parsePort)
 <|> ((string "gamekind") *> delimParser *> parseGamekind)

lineParser :: Parser [Product]
lineParser = many $ productParser <* endOfLine

delimParser :: Parser ()
delimParser = skipSpace *> (string "=") *> skipSpace

commentParser :: Parser Product
commentParser = ((char '#') *> skipWhile (not.isEndOfLine)) *> endOfLine *> productParser

parseHostname :: Parser Product
parseHostname = do
    str <- takeWhile1 (not.isEndOfLine)
    return $ Hostname (unpack $ str)

parsePort :: Parser Product
parsePort = do
    port <- decimal
    return $ Port port

parseGamekind :: Parser Product
parseGamekind = do
    kind <- takeWhile1 (not.isEndOfLine)
    return $ Gamekind (unpack $ kind)

main :: IO ()
main = do
  print $ parseOnly lineParser "hostname=Host\n#test\nport=345\r\ngamekind=test\n"