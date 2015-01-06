{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module NineMorris.Globals where

import Data.Text (Text)
import Control.Exception
import Data.Typeable
import qualified Data.Map as Map

defaultConfig   = "config.ini"  :: String
gameIdLength    = 11            :: Int
internalVersion = "1.0"         :: Text
playerNumber    = "1"           :: Text
searchDepth     = 2             :: Int
positions       = [ "A0","A1","A2",
                    "B0","B1","B2",
                    "C0","C1","C2",
                    "A7","B7","C7",
                    "C3","B3","A3",
                    "C6","C5","C4",
                    "B6","B5","B4",
                    "A6","A5","A4" ] :: [Text]

toAiPositions     = Map.fromList $ zipWith (\a b -> (a,b)) positions ([0..23]::[Int])
toServerPositions = Map.fromList $ zipWith (\a b -> (b,a)) positions ([0..23]::[Int])

data Config         = Config {hostname::Text, port::Int, gamekind::Text} deriving (Show)
data PlayerInfo     = PlayerInfo {pid::Int, pname::Text, pstatus::PlayerStatus} deriving (Show)
data StoneInfo      = StoneInfo {spid::Int, snumber::Int, sposition::Text} deriving (Show)
data PlayerStatus   = READY | NOT_READY deriving (Show)
data GamePhase      = GP_WAIT | GP_MOVE Int | GP_GAMEOVER (Maybe (Int, Text))
type Gameid         = String

{- Game Excemptions -}
data MorrisException = GameIdNotValid | FileNotFound | ConfigNotValid Text | ProtocolError Text | InternalParserError String | AiException
    deriving (Show, Typeable)

instance Exception MorrisException