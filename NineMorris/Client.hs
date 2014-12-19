module NineMorris.Client (startClient) where

import Network.Socket

type Gameid = String
data Config = Config {hostname::String, port::Int, gamekind::String}

startClient :: String -> String -> IO ()
startClient gid cnf = performConnection (verifyGameId gid) (readConfigFile cnf)
{-
    opens, handels and closes TCP connection to gameserver
-}
performConnection :: Gameid -> Config -> IO ()
performConnection = undefined

readConfigFile :: String -> Config
readConfigFile path = undefined

verifyGameId :: String -> Gameid
verifyGameId gid = gid