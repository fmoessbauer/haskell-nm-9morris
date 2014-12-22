module NineMorris.Client (startClient) where

import qualified NineMorris.Globals as G
import Control.Exception
import Network.Socket
import qualified Data.Map as Map
import qualified NineMorris.Parsers.Config as ConfigParser

type Gameid = String

startClient :: String -> String -> IO ()
startClient gid cnf = do
  valgid <- return $! verifyGameId gid
  config <- readConfigFile cnf
  performConnection valgid config

{-
    opens, handels and closes TCP connection to gameserver
-}
performConnection :: Gameid -> G.Config -> IO ()
performConnection gid cnf = do
  putStrLn $ "gameid: "++gid++" "++ (show $ cnf)

{-
    Method to parse a config file. Temporarily hardcoded input
-}
readConfigFile :: String -> IO G.Config
readConfigFile path = do
  content <- readFile path
  return $ ConfigParser.getConfig content

verifyGameId :: String -> Gameid
verifyGameId gid = if (length $ gid) == G.gameIdLength
                      then gid
                      else throw G.GameIdNotValid