{-# LANGUAGE OverloadedStrings #-}
module NineMorris.Connector (performConnection) where

import qualified NineMorris.Globals as G
import Network.Socket
import System.IO
import Control.Exception
import Control.Monad
import Data.Text (Text,pack,unpack,append)
import qualified Data.Text.IO as TextIO
import NineMorris.Parsers.Protocol

performConnection :: G.Gameid -> G.Config -> IO ()
performConnection gid cnf@G.Config{G.hostname=hostname, G.port=port, G.gamekind=gamekind} = do
    -- debug
    putStrLn $ "gameid: "++gid++" "++ (show $ cnf)

    -- create socket
    let hints = defaultHints { addrFlags = [AI_CANONNAME], addrSocketType = Stream}
    addrInfos <- getAddrInfo (Just hints) (Just (unpack $ hostname)) (Just $ show $ port)

    -- debug dns request
    --putStrLn $ show $ addrInfos

    let addr = head addrInfos
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    -- Connect
    connect sock (addrAddress addr)
    -- convert to handle
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    -- ready for IO
    -- Todo: Handle Exceptions
    handleProtocol gid gamekind hdl

    -- close Handle and Socket
    hClose hdl

handleProtocol :: G.Gameid -> Text ->Handle -> IO ()
handleProtocol gid gkind hdl = do
    handleProlog gid gkind hdl
    return ()

handleProlog :: G.Gameid -> Text -> Handle -> IO ()
handleProlog gid gkind hdl = do
    line <- getDebugLine hdl
    let vers = parseWelcome line
    -- Todo check version

    -- send Client version
    putDebugStrLn hdl $ "VERSION " `append` G.internalVersion
    getDebugLine hdl >>= parseClientVersOk

    -- send Gameid
    putDebugStrLn hdl $ pack $ "ID "++gid

    -- recieve Gamekind or error
    getDebugLine hdl >>= (\str -> parseGamekindOk str gkind)

    -- recieve Game name
    gamename <- (getDebugLine hdl >>= (\str -> return $ parseGameName str))

    -- send player number
    putDebugStrLn hdl $ "PLAYER " `append` G.playerNumber

    -- get player info
    getDebugLine hdl
    total <- getDebugLine hdl >>= (\str -> return $ parseTotalPlayer str)
    players <- replicateM (total-1) (getDebugLine hdl >>= (\str -> return $ parsePlayerInfo str))
    putStrLn $ show $ players

    -- recieve endplayers string
    getDebugLine hdl >>= parseEndplayers


parseClientVersOk :: Text -> IO ()
parseClientVersOk str = if str == "+ Client version accepted - please send Game-ID to join"
    then return ()
    else throw $ G.ProtocolError "Client version not ok"

parseGamekindOk :: Text -> Text -> IO ()
parseGamekindOk str gkind = 
    let gamekind = parseGamekind str
    in if (gamekind == gkind)
        then return ()
        else throw $ G.ProtocolError ("Gamekind not valid: " `append` gamekind)

parseEndplayers :: Text -> IO ()
parseEndplayers str = if str == "+ ENDPLAYERS"
    then return ()
    else throw $ G.ProtocolError ("ENDPLAYERS expected, but: " `append` str)
getDebugLine :: Handle -> IO Text
getDebugLine hdl = do
    line <- TextIO.hGetLine hdl
    putStrLn $ "DEBUG: "++(show $ line)
    return line

putDebugStrLn :: Handle -> Text -> IO ()
putDebugStrLn hdl str = do
    putStrLn $ "DEBUG: "++(unpack $ str)
    TextIO.hPutStrLn hdl str
    return ()