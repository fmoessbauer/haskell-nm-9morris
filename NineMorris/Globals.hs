{-# LANGUAGE DeriveDataTypeable #-}
module NineMorris.Globals where

import Control.Exception
import Data.Typeable

defaultConfig   = "config.ini"  :: String
gameIdLength    = 11            :: Int

{- Game Excemptions -}
data MorrisException = GameIdNotValid | FileNotFound
    deriving (Show, Typeable)

instance Exception MorrisException