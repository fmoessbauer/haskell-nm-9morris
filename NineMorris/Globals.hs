{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.Globals
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module is for an easy access to global constants for the
-- NineMorris client. It is recommented to import it qualified
-----------------------------------------------------------------------------
module NineMorris.Globals where

import Data.Text (Text)
import Control.Exception
import Data.Typeable
import qualified Data.Map as Map

-- | reconnect if connection is lost. Due to the game laws this functionality is not allowed and so currently not implemented
allowReconnect :: Bool
allowReconnect  = True

-- | path to default config file
defaultConfig :: String
defaultConfig   = "config.ini"

-- | if no player is specified use this. Possible values are empty string, 0, 1
defaultPlayer :: String
defaultPlayer   = ""

-- | length of gameid. Do not change!
gameIdLength :: Int
gameIdLength    = 11            :: Int

-- | internal client version. First digit has to match server version
internalVersion :: Text
internalVersion = "1.0"

-- | minimal search depth. Possible values >= 0
searchDepth :: Int
searchDepth     = 1

-- | maximal search depth. Must be >= searchDepth
maxSearchDepth :: Int
maxSearchDepth  = 20

-- | difference between maximum think time and timeout fires in ms. Only positive values
aiTimeoutBuffer :: Int
aiTimeoutBuffer = 200

-- | ordered list of positions transmitted by server.
positions :: [Text]
positions       = [ "A0","A1","A2",
                    "B0","B1","B2",
                    "C0","C1","C2",
                    "A7","B7","C7",
                    "C3","B3","A3",
                    "C6","C5","C4",
                    "B6","B5","B4",
                    "A6","A5","A4" ]

-- | Data.Map with key server position and value internal AI position. Do not change!
toAiPositions :: Map.Map Text Int
toAiPositions     = Map.fromList $ zipWith (\a b -> (a,b)) positions ([0..23]::[Int])

-- | Data.Map with key internal AI position and value server position. Do not change!
toServerPositions :: Map.Map Int Text
toServerPositions = Map.fromList $ zipWith (\a b -> (b,a)) positions ([0..23]::[Int])

-- | default playerInfo record to prevent compiler warning when not initializing all fields
defaultPlayerInfo :: PlayerInfo
defaultPlayerInfo = PlayerInfo {
        pid     = -1,
        pname   = "",
        pstatus = NOT_READY
    }

-- | flag which ai is used. Do not change!
-- this seems to be redundant, but I guess that's the only way to achive low coupling
aiType :: TypeOfAI
aiType          =
#if FUNCTIONALAI
    SIMPLE
#else
    CLEVER
#endif

-- | flag if program is compiled with debug mode on
isDebug :: Bool
isDebug         =
#if DEBUG
    True
#else
    False
#endif

-- | config record
data Config         = Config {hostname::Text, port::Int, gamekind::Text} deriving (Show)

-- | player information
data PlayerInfo     = PlayerInfo {
        pid::Int,               -- ^ id
        pname::Text,            -- ^ name of player
        pstatus::PlayerStatus   -- ^ status
    } deriving (Show)

data StoneInfo      = StoneInfo {spid::Int, snumber::Int, sposition::Text} deriving (Show)
data PlayerStatus   = READY | NOT_READY deriving (Show)

-- | in which game phase we are
data GamePhase      = GP_WAIT                           -- ^ opponent is playing, we have to wait
                    | GP_MOVE Int                       -- ^ it's our turn
                    | GP_GAMEOVER (Maybe (Int, Text))   -- ^ gameover and winner / draw information is given

type Gameid         = String

data TypeOfAI       = SIMPLE -- ^ used simple AI
                    | CLEVER -- ^ used clever AI
                        deriving (Show)

-- | Game Exceptions
data MorrisException = GameIdNotValid | PlayerIdNotValid | FileNotFound | ConfigNotValid Text | ProtocolError Text | InternalParserError String | AiException | TimeOutAI
    deriving (Show, Typeable)

instance Exception MorrisException
