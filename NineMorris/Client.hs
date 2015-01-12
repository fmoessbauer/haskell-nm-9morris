module NineMorris.Client (startClient) where

import qualified NineMorris.Globals as G
import Control.Exception
import Data.Map()
import qualified NineMorris.Parsers.Config as ConfigParser
import NineMorris.Connector

startClient :: String -> String -> String -> IO ()
startClient gid cnf pid = do
  valgid <- return $! verifyGameId gid
  player <- return $! verifyPlayerId pid
  config <- readConfigFile cnf
  performConnection valgid config player

{-
    opens, handels and closes TCP connection to gameserver
-}
{-
performConnection :: Gameid -> G.Config -> IO ()
performConnection gid cnf = do
  putStrLn $ "gameid: "++gid++" "++ (show $ cnf)
-}
{-
    Method to parse a config file. Temporarily hardcoded input
-}
readConfigFile :: String -> IO G.Config
readConfigFile path = do
  content <- readFile path
  return $ ConfigParser.getConfig content

verifyGameId :: String -> G.Gameid
verifyGameId gid = if (length $ gid) == G.gameIdLength
                      then gid
                      else throw G.GameIdNotValid
                      
verifyPlayerId :: String -> Maybe Int
verifyPlayerId pl = 
    case pl of
         ""   -> Nothing
         "0"  -> Just 0
         "1"  -> Just 1
         _    -> throw G.PlayerIdNotValid