{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module NineMorris.Globals where

import Data.Text (Text)
import Control.Exception
import Data.Typeable

defaultConfig   = "config.ini"  :: String
gameIdLength    = 11            :: Int
internalVersion = "1.0"         :: Text
playerNumber    = "1"           :: Text

data Config         = Config {hostname::Text, port::Int, gamekind::Text} deriving (Show)
data PlayerInfo     = PlayerInfo {pid::Int, pname::Text, pstatus::PlayerStatus} deriving (Show)
data StoneInfo      = StoneInfo {spid::Int, snumber::Int, sposition::Text} deriving (Show)
data PlayerStatus   = READY | NOT_READY deriving (Show)
data GamePhase      = GP_WAIT | GP_MOVE Int | GP_GAMEOVER (Maybe (Int, Text))
type Gameid         = String

{- Game Excemptions -}
data MorrisException = GameIdNotValid | FileNotFound | ConfigNotValid Text | ProtocolError Text | InternalParserError
    deriving (Show, Typeable)

instance Exception MorrisException