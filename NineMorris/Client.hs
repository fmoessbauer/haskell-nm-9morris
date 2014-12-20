module NineMorris.Client (startClient) where

import qualified NineMorris.Globals as Globals
import Control.Exception
import Network.Socket

type Gameid = String
data Config = Config {hostname::String, port::Int, gamekind::String} deriving (Show)

startClient :: String -> String -> IO ()
startClient gid cnf = do
  valgid <- return $! verifyGameId gid
  config <- readConfigFile cnf
  performConnection valgid config

{-
    opens, handels and closes TCP connection to gameserver
-}
performConnection :: Gameid -> Config -> IO ()
performConnection gid cnf = do
  putStrLn $ "gameid: "++gid++" "++ (show $ cnf)

{-
    Method to parse a config file. Temporarily hardcoded input
-}
readConfigFile :: String -> IO Config
readConfigFile path = do
  let conf = Config {
      hostname  = "sysprak.priv.lab.nm.ifi.lmu.de",
      port      = 1357,
      gamekind  = "NMMorris"
    }
  return $ conf

verifyGameId :: String -> Gameid
verifyGameId gid = if (length $ gid) == Globals.gameIdLength
                      then gid
                      else throw Globals.GameIdNotValid