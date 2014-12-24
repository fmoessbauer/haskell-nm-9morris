{-# LANGUAGE DeriveDataTypeable #-}
module NineMorris.Globals where

import Control.Exception
import Data.Typeable

defaultConfig   = "config.ini"  :: String
gameIdLength    = 11            :: Int
internalVersion = "1.0"         :: String

data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)
type Gameid = String

{- Game Excemptions -}
data MorrisException = GameIdNotValid | FileNotFound | ConfigNotValid String | ProtocolError String
    deriving (Show, Typeable)

instance Exception MorrisException