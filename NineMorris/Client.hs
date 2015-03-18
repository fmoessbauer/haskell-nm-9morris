-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.Client
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- Haskell Nine Men Morris client
-----------------------------------------------------------------------------
module NineMorris.Client (startClient) where

import qualified NineMorris.Globals as G
import Control.Exception
import Data.Map()
import qualified NineMorris.Parsers.Config as ConfigParser
import NineMorris.Connector

-- | start the nine men morris client
startClient :: String   -- ^ gameid of choosen game
            -> String   -- ^ config file path
            -> String   -- ^ prefered player id or empty string
            -> IO ()
startClient gid cnf pid = do
  valgid <- return $! convertGameId gid
  player <- return $! convertPlayerId pid
  config <- readConfigFile cnf
  catch (performConnection valgid config player) (expHandler)

-- | Method to parse a config file.
readConfigFile :: String        -- ^ file path
               -> IO G.Config
readConfigFile path = do
  content <- readFile path
  return $ ConfigParser.getConfig content

-- | verifies and converts the given game id
convertGameId :: String -> G.Gameid
convertGameId gid = if (length $ gid) == G.gameIdLength
                      then gid
                      else throw G.GameIdNotValid

-- | verifies and converts the given player                  
convertPlayerId :: String -> Maybe Int
convertPlayerId pl = 
    case pl of
         ""   -> Nothing
         "0"  -> Just 0
         "1"  -> Just 1
         _    -> throw G.PlayerIdNotValid
         
expHandler :: G.MorrisException -> IO ()
expHandler e = putStrLn $ "[FATAL ERROR]: " ++ (show e)
