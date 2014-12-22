{-# LANGUAGE DeriveDataTypeable #-}
module NineMorris.Globals where

import Control.Exception
import Data.Typeable

defaultConfig   = "config.ini"  :: String
gameIdLength    = 11            :: Int

data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)

{- Game Excemptions -}
data MorrisException = GameIdNotValid | FileNotFound | ConfigNotValid
    deriving (Show, Typeable)

instance Exception MorrisException