{-# LANGUAGE DeriveDataTypeable #-}
module NineMorris.Globals where

import Data.Text (Text)
import Control.Exception
import Data.Typeable

defaultConfig   = "config.ini"  :: String
gameIdLength    = 11            :: Int
internalVersion = "1.0"         :: String

data Config = Config {hostname::Text, port::Int, gamekind::Text} deriving (Show)
type Gameid = String

{- Game Excemptions -}
data MorrisException = GameIdNotValid | FileNotFound | ConfigNotValid Text | ProtocolError Text
    deriving (Show, Typeable)

instance Exception MorrisException