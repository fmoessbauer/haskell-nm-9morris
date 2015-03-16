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
  parseStatic,
  -- * Prolog phase parsers
  parseWelcome,
  parseGamekind,
  parseGameName,
  parseTotalPlayer,
  parsePlayerInfo,
  parseMePlayerInfo,
  -- * Phase control parsers
  parseGPSwitch,
  -- * Move phase parsers
  parseMoveCapture,
  parseMovePieces,
  parseMoveStoneData)
where

import qualified NineMorris.Globals as G
import Data.Attoparsec.Text
import Data.Text (unpack, snoc, Text) 
import Control.Applicative
import Control.Exception

-- | major, minor version
type Version = (Int,Int)

parserWelcome :: Parser Version
parserWelcome = do
      (string "+ MNM Gameserver v")
      mayor <- decimal
      (char '.')
      minor <- decimal
      (string  " accepting connections")
      return $ (mayor,minor)

-- | parses the gameserver welcome message and returns the version
parseWelcome :: Text -> Version
parseWelcome str = normalizedParse $ parseOnly parserWelcome str

parserGamekind :: Parser Text
parserGamekind = (string "+ PLAYING ") *>  takeWhile1 (not.isEndOfLine)

-- | parses the gamekind the server wants to play.
parseGamekind :: Text -> Text
parseGamekind str = normalizedParse $ parseOnly parserGamekind str

-- | parses the name of the game
parseGameName :: Text -> Text
parseGameName str = normalizedParse $ parseOnly ((string "+ ") *> (takeWhile1 (not.isEndOfLine))) str

-- | parses the number of players taking part in this game
parseTotalPlayer :: Text -> Int
parseTotalPlayer str = normalizedParse $ parseOnly ((string "+ TOTAL ") *> (decimal)) str

-- | see description at parsePlayerInfo
parserPlayerInfo :: Parser G.PlayerInfo
parserPlayerInfo = do
    string "+ "
    skipSpace
    pid <- decimal
    skipSpace
    parseRemaining (G.defaultPlayerInfo {G.pid = pid}) "" Nothing
    where
        parseRemaining :: G.PlayerInfo -> Text -> (Maybe Char) -> Parser G.PlayerInfo
        parseRemaining rec akk c = do
            sp <- anyChar
            lp <- peekChar  -- lookahead if sp is last char
            case lp of
                 Nothing -> return rec {
                        G.pname     = akk,
                        G.pstatus   = case sp of
                                           '1' -> G.READY
                                           _   -> G.NOT_READY
                    }
                 Just _  -> case c of
                                 Nothing -> parseRemaining rec akk (Just sp)
                                 Just ca -> parseRemaining rec (akk `snoc` ca) (Just sp)
                 

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

-- | parses a single player record, containing player id and name
--   
--   Some words to the weird looking implemenation:
--   The string to be parsed looks like "+ YOU 0 My Name with spaces 0" where the last number
--   after YOU is the pid and the last char is the player number. 
parsePlayerInfo :: Text -> G.PlayerInfo
parsePlayerInfo str = normalizedParse $ parseOnly parserPlayerInfo str
    
-- | parses the client player information (that's me)
parseMePlayerInfo :: Text -> G.PlayerInfo
parseMePlayerInfo str = normalizedParse $ parseOnly parserMePlayerInfo str

parserGPSwitch :: Parser G.GamePhase
parserGPSwitch = 
  ((string "+ WAIT") *> (return $ G.GP_WAIT))
  <|> ((string "+ MOVE ") *> (decimal >>= (\nr -> return $ G.GP_MOVE nr)))
  <|> ((string "+ GAMEOVER") *> skipSpace *> (parserGameOver <|> parserGameOverDraw))

-- | parses the gamephase string
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
-- | parses the stone capture information
parseMoveCapture :: Text -> Int
parseMoveCapture str = normalizedParse $ parseOnly ((string "+ CAPTURE ") *> (decimal)) str

parserMovePieces :: Parser (Int,Int)
parserMovePieces = do
    string "+ PIECELIST "
    players <- decimal
    char ','
    stones <- decimal
    return $ (players, stones)

-- | parses the number of pieces (me and opponent)
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

-- | parses a single raw piece to a StoneInfo record
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
