{-# LANGUAGE OverloadedStrings #-}
module NineMorris.Connector (performConnection) where

import qualified NineMorris.Globals as G
import Network.Socket
import System.IO
import Control.Exception
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Text (Text,pack,unpack,append)
import qualified Data.Text.IO as TextIO
import NineMorris.Parsers.Protocol
import qualified NineMorris.AI as AI
import NineMorris.AI.Interface

import qualified Data.Map as Map

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
    player <- handleProlog gid gkind hdl
    handleGamePhase hdl player
    return ()

handleProlog :: G.Gameid -> Text -> Handle -> IO G.PlayerInfo
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
    mePlayer <- getDebugLine hdl >>= (\str -> return $ parseMePlayerInfo str)

    total <- getDebugLine hdl >>= (\str -> return $ parseTotalPlayer str)
    -- todo: check positiv response
    players <- replicateM (total-1) (getDebugLine hdl >>= (\str -> return $ parsePlayerInfo str))
    putStrLn $ show $ players

    -- recieve endplayers string
    getDebugLine hdl >>= parseEndplayers

    putStrLn $ show $ mePlayer
    return $ mePlayer

handleGamePhase :: Handle -> G.PlayerInfo -> IO ()
handleGamePhase hdl player = fix $ \loop -> do
    line <- getDebugLine hdl
    ret <- case parseGPSwitch $ line of
        G.GP_WAIT          -> putDebugStrLn hdl "OKWAIT" >> return True
        G.GP_MOVE time     -> movePhase hdl player time
        G.GP_GAMEOVER dat  -> gameOver hdl dat
    if ret then loop else return ()

movePhase :: Handle -> G.PlayerInfo -> Int -> IO Bool
movePhase hdl player time = do
    putStrLn $ "Begin MovePhase"
    putStrLn $ "Player:" ++ (show $ player)
    
    pieces <- getPieceInfo hdl

    let board = AI.setBoardNextPlayer AI.Black $ convertBoard player pieces
    putStrLn $ show $ board

    --putStrLn $ show $ map (\n -> show $ AI.getBoardPosition (AI.Position n) board) [0..23]
    --putStrLn $ show $ AI.getBoardHandCount AI.Red board

    getDebugLine hdl >>= parseStatic "+ ENDPIECELIST"
    putDebugStrLn hdl "THINKING"
    getDebugLine hdl >>= parseStatic "+ OKTHINK"
    -- Play useless
    -- calculate move
    --  BIG TODO
    --
    let intMove = AI.aiMove G.searchDepth Map.empty board
    putStrLn $ show $ intMove

    move <- return $ convertMove $ intMove
    putDebugStrLn hdl ("PLAY " `append` move)
    getDebugLine hdl >>= parseStatic "+ MOVEOK"
    return True

gameOver :: Handle -> (Maybe (Int,Text)) -> IO Bool
gameOver hdl winner = do
    -- remove duplicate code TODO
    case winner of
        Just (number, name) -> putStrLn $ "Player #" ++ (show $ number) ++ " with name " ++ (unpack $ name) ++ " won the match"
        Nothing             -> putStrLn $ "Gameover with draw"
    getPieceInfo hdl
    getDebugLine hdl >>= parseStatic "+ QUIT"
    --
    hClose hdl
    --exitWith ExitSuccess
    return False

getPieceInfo :: Handle -> IO [G.StoneInfo]
getPieceInfo hdl = do
    getDebugLine hdl >>= (\str -> return $ parseMoveCapture str) --no capture needed
    (cntPlayer, cntStones)  <- getDebugLine hdl >>= (\str -> return $ parseMovePieces str)
    pieces <- replicateM (cntPlayer*cntStones) (getDebugLine hdl >>= (\str -> return $ parseMoveStoneData str))
    getDebugLine hdl >>= parseStatic "+ ENDPIECELIST"

    putStrLn $ show $ pieces
    
    return pieces

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