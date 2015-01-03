{-# LANGUAGE OverloadedStrings #-}
module NineMorris.Connector (performConnection) where

import qualified NineMorris.Globals as G
import Network.Socket
import System.IO
import Control.Exception
import Control.Monad
import Control.Monad.Fix (fix)
import System.Exit (exitWith, ExitCode(..))
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
    handleGamePhase hdl
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
    -- todo: check positiv response
    players <- replicateM (total-1) (getDebugLine hdl >>= (\str -> return $ parsePlayerInfo str))
    putStrLn $ show $ players

    -- recieve endplayers string
    getDebugLine hdl >>= parseEndplayers

handleGamePhase :: Handle -> IO ()
handleGamePhase hdl = fix $ \loop -> do
    line <- getDebugLine hdl
    ret <- case parseGPSwitch $ line of
        G.GP_WAIT          -> putDebugStrLn hdl "OKWAIT" >> return True
        G.GP_MOVE time     -> movePhase hdl time
        G.GP_GAMEOVER dat  -> gameOver hdl dat
    if ret then loop else return ()

movePhase :: Handle -> Int -> IO Bool
movePhase hdl time = do
    capture <- getDebugLine hdl >>= (\str -> return $ parseMoveCapture str)
    (cntPlayer, cntStones)  <- getDebugLine hdl >>= (\str -> return $ parseMovePieces str)
    pieces <- replicateM (cntPlayer*cntStones) (getDebugLine hdl >>= (\str -> return $ parseMoveStoneData str))
    --putStrLn $ show $ pieces
    getDebugLine hdl >>= parseStatic "+ ENDPIECELIST"
    putDebugStrLn hdl "THINKING"
    getDebugLine hdl >>= parseStatic "+ OKTHINK"
    -- Play useless
    -- calculate move
    --  BIG TODO
    --
    move <- return $ "A1"
    putDebugStrLn hdl ("PLAY " `append` move)
    getDebugLine hdl >>= parseStatic "+ MOVEOK"
    return True

gameOver :: Handle -> (Maybe (Int,Text)) -> IO Bool
gameOver hdl _ = do
    -- remove duplicate code TODO
    capture <- getDebugLine hdl >>= (\str -> return $ parseMoveCapture str)
    (cntPlayer, cntStones)  <- getDebugLine hdl >>= (\str -> return $ parseMovePieces str)
    pieces <- replicateM (cntPlayer*cntStones) (getDebugLine hdl >>= (\str -> return $ parseMoveStoneData str))
    getDebugLine hdl >>= parseStatic "+ ENDPIECELIST"
    getDebugLine hdl >>= parseStatic "+ QUIT"
    --
    hClose hdl
    --exitWith ExitSuccess
    return False


-- if idle then respond else return line
idleResponse :: Handle -> IO Text
idleResponse hdl = do
    line <- getDebugLine hdl
    if (line == "+ WAIT")
        then (putDebugStrLn hdl "OKWAIT") >> idleResponse hdl
        else return line

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
parseEndplayers str = if str == "+ ENDPLAYERS" -- todo switch to parseStatic
    then return ()
    else throw $ G.ProtocolError ("ENDPLAYERS expected, but: " `append` str)

getDebugLine :: Handle -> IO Text
getDebugLine hdl = do
    line <- TextIO.hGetLine hdl
    putStrLn $ "DEBUG: "++(show $ line)
    return line

putDebugStrLn :: Handle -> Text -> IO ()
putDebugStrLn hdl str = do
    TextIO.hPutStrLn hdl str
    putStrLn $ "DEBUG: "++(unpack $ str)
    return ()