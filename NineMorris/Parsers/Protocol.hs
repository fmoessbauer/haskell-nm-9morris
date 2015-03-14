{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.Parsers.Protocol
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides functions to parse strings from the server
-- to different types.
-- the parser... functions are the actual parsers. The parse... functions
-- are wrappers to get easier access to the parsed data in the protocol
-- modules
-----------------------------------------------------------------------------
module NineMorris.Parsers.Protocol (
  Version,
  parseWelcome,
  parseGamekind,
  parseGameName,
  parseTotalPlayer,
  parsePlayerInfo,
  parseMePlayerInfo,
  parseGPSwitch,
  parseMoveCapture,
  parseStatic,
  parseMovePieces,
  parseMoveStoneData)
where

import qualified NineMorris.Globals as G
import Data.Attoparsec.Text
import Data.Text (unpack, take, length, last, Text) 
import Control.Applicative
import Control.Exception

type Version = (Int,Int)

parserWelcome :: Parser Version
parserWelcome = do
      (string "+ MNM Gameserver v")
      mayor <- decimal
      (char '.')
      minor <- decimal
      (string  " accepting connections")
      return $ (mayor,minor)

parseWelcome :: Text -> Version
parseWelcome str = normalizedParse $ parseOnly parserWelcome str

parserGamekind :: Parser Text
parserGamekind = (string "+ PLAYING ") *>  takeWhile1 (not.isEndOfLine)

parseGamekind :: Text -> Text
parseGamekind str = normalizedParse $ parseOnly parserGamekind str

parseGameName :: Text -> Text
parseGameName str = normalizedParse $ parseOnly ((string "+ ") *> (takeWhile1 (not.isEndOfLine))) str

parseTotalPlayer :: Text -> Int
parseTotalPlayer str = normalizedParse $ parseOnly ((string "+ TOTAL ") *> (decimal)) str

parserPlayerInfo :: Parser (Int, Text)
parserPlayerInfo = do
    (string "+ ")
    -- parse player number
    pnr <- decimal
    skipSpace
    -- parse player name
    pname <- takeText
    return $ (pnr, pname)

parserMePlayerInfo :: Parser G.PlayerInfo
parserMePlayerInfo = do
  string "+ YOU"
  skipSpace
  pid <- decimal
  pname <- takeText
  return $ G.PlayerInfo{
        G.pid     = pid,
        G.pname   = pname,
        G.pstatus = G.READY
      }

-- very bad implemenation
-- TODO!!
parsePlayerInfo :: Text -> G.PlayerInfo
parsePlayerInfo str = let (nr, name) = normalizedParse $ parseOnly parserPlayerInfo (Data.Text.take ((Data.Text.length $ str)-2) $ str)
  in G.PlayerInfo{
      G.pid     = nr,
      G.pname   = name,
      G.pstatus = if (Data.Text.last $ str) == '1'
        then G.READY
        else G.NOT_READY
    }

parseMePlayerInfo :: Text -> G.PlayerInfo
parseMePlayerInfo str = normalizedParse $ parseOnly parserMePlayerInfo str

parserGPSwitch :: Parser G.GamePhase
parserGPSwitch = 
  ((string "+ WAIT") *> (return $ G.GP_WAIT))
  <|> ((string "+ MOVE ") *> (decimal >>= (\nr -> return $ G.GP_MOVE nr)))
  <|> ((string "+ GAMEOVER") *> skipSpace *> (parserGameOver <|> parserGameOverDraw))

parseGPSwitch :: Text -> G.GamePhase
parseGPSwitch str = normalizedParse $ parseOnly parserGPSwitch str

parserGameOver :: Parser G.GamePhase
parserGameOver = do
  pid <- decimal
  skipSpace
  name <- takeText
  return $ G.GP_GAMEOVER (Just (pid, name))

parserGameOverDraw :: Parser G.GamePhase
parserGameOverDraw = return $ G.GP_GAMEOVER Nothing

{- move phase parsers -}
parseMoveCapture :: Text -> Int
parseMoveCapture str = normalizedParse $ parseOnly ((string "+ CAPTURE ") *> (decimal)) str

parserMovePieces :: Parser (Int,Int)
parserMovePieces = do
    string "+ PIECELIST "
    players <- decimal
    char ','
    stones <- decimal
    return $ (players, stones)

parseMovePieces :: Text -> (Int,Int)
parseMovePieces str = normalizedParse $ parseOnly parserMovePieces str

parserMoveStoneData :: Parser G.StoneInfo
parserMoveStoneData = do
    string "+ PIECE"
    pnr <- decimal
    char '.'
    stone <- decimal
    skipSpace
    pos <- takeWhile1 (not.isHorizontalSpace)
    return G.StoneInfo {G.spid=pnr, G.snumber=stone, G.sposition=pos}

parseMoveStoneData :: Text -> G.StoneInfo
parseMoveStoneData str = normalizedParse $ parseOnly parserMoveStoneData str
{- end move phase parsers-}

-- | parse a static text
--   returns IO () if parser passes and exception if parser fails 
parseStatic :: Text   -- ^ expected text
            -> Text   -- ^ actual text
            -> IO ()  -- ^ exception if parser fails
parseStatic expected str = if expected == str then return () else throw $ G.InternalParserError ("No Parse: " ++ (unpack $ str))
--(return $ (normalizedParse $ parseOnly (string expected) str)) >> return () -- bad implemenation

-- | return the result if parser passes and exception if parser failes
normalizedParse :: Either String a -> a
normalizedParse (Right a) = a
normalizedParse (Left str)  = throw $ G.InternalParserError str
