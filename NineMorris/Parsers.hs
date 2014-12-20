{-# LANGUAGE OverloadedStrings #-}
{- Standalone parser implementation -}
module NineMorris.Parsers where

--import NineMorris.Global as Global
import Data.Attoparsec.Text
import Data.Text
import Control.Applicative

data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)
data Product = Hostname String | Port Int | Gamekind String | Test deriving (Show)

productParser :: Parser Product
productParser =
     ((string "hostname"    >> return Hostname) *> parseHostname)
 <|> ((string "port" >> return Port) *> parsePort)
 <|> ((string "gamekind"  >> return Gamekind ) *> parseGamekind)

lineParser :: Parser [Product]
lineParser = many $ productParser <* endOfLine

main :: IO ()
main = do
  print $ parseOnly lineParser "port345\r\n"

parseHostname :: Parser Product
parseHostname = do
    str <- takeText
    return $ Hostname (unpack $ str)

parsePort :: Parser Product
parsePort = do
    port <- decimal
    return $ Port port

parseGamekind :: Parser Product
parseGamekind = do
    kind <- takeText
    return $ Gamekind (unpack $ kind)