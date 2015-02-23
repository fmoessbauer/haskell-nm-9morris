{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.Connector
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module handles the connection to the server,
-- performes the protocol and drives the AI Interface
-----------------------------------------------------------------------------
module NineMorris.Connector (performConnection) where

import qualified NineMorris.Globals as G
import NineMorris.Globals (Config(..))
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Timer as Timer
import Control.Concurrent.Suspend (msDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Text (Text,pack,unpack,append)
import qualified Data.Text.IO as TextIO
import NineMorris.Parsers.Protocol

#if FUNCTIONALAI
import NineMorris.AI.SimpleInterface
#else
import NineMorris.AI.CleverInterface
#endif

-- | establish the connection to the game server
performConnection :: G.Gameid   -- ^ gameid of choosen game
                  -> Config     -- ^ config record with server information
                  -> Maybe Int  -- ^ prefered player id or Nothing
                  -> IO ()
performConnection gid cnf@Config{hostname, port, gamekind} player = do
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
    handleProtocol gid gamekind player hdl

    -- close Handle and Socket
    hClose hdl

-- | handle the protocol
handleProtocol :: G.Gameid      -- ^ gameid of choosen game
               -> Text          -- ^ gamekind of the AI.
               -> Maybe Int     -- ^ prefered player id or Nothing
               -> Handle        -- ^ socket handle
               -> IO ()
handleProtocol gid gkind wishPlayer hdl = do
    player <- handleProlog gid gkind wishPlayer hdl
    handleGamePhase hdl player
    return ()

-- | handle the prolog protocol phase
handleProlog :: G.Gameid -> Text -> Maybe Int -> Handle -> IO G.PlayerInfo
handleProlog gid gkind wishPlayer hdl = do
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
    let wishPid = case wishPlayer of
                   Nothing  -> ""
                   Just pid -> pack $ show $ pid
    putDebugStrLn hdl $ "PLAYER " `append` wishPid

    -- get player info
    mePlayer <- getDebugLine hdl >>= (\str -> return $ parseMePlayerInfo str)

    total <- getDebugLine hdl >>= (\str -> return $ parseTotalPlayer str)
    -- todo: check positiv response
    players <- replicateM (total-1) (getDebugLine hdl >>= (\str -> return $ parsePlayerInfo str))
    putStrLn $ show $ players

    -- recieve endplayers string
    getDebugLine hdl >>= parseEndplayers

    putStrLn $ "Playing game '" ++ (unpack $ gamename) ++ "' with " ++ (show $ total) ++ " players"
    putStrLn $ show $ mePlayer
    return $ mePlayer

-- | loop to handle the game phase.
handleGamePhase :: Handle -> G.PlayerInfo -> IO ()
handleGamePhase hdl player = fix $ \loop -> do  -- prevents memory leaks
    line <- getDebugLine hdl
    ret <- case parseGPSwitch $ line of
        G.GP_WAIT          -> putDebugStrLn hdl "OKWAIT" >> return True
        G.GP_MOVE time     -> movePhase hdl player time
        G.GP_GAMEOVER dat  -> gameOver hdl player dat
    if ret then loop else return ()

movePhase :: Handle -> G.PlayerInfo -> Int -> IO Bool
movePhase hdl player time = do
    putStrLn $ "Begin MovePhase"
    putStrLn $ "Player:" ++ (show $ player)
    
    pieces <- getPieceInfo hdl

    let board = convertBoard player pieces
    putStrLn $ show $ board

    --putStrLn $ show $ map (\n -> show $ AI.getBoardPosition (AI.Position n) board) [0..23]
    --putStrLn $ show $ AI.getBoardHandCount AI.Red board

    putDebugStrLn hdl "THINKING"
    --
    moveStore <- newEmptyMVar
    moveSave  <- newMVar (Nothing, 0)
    --
    
    tid <- forkIO $ handle timeoutHandler $ calculateIterativeMove (moveStore,moveSave) board 0
    
    Timer.oneShotTimer (do
      throwTo tid G.TimeOutAI
      (intMove,depth) <- takeMVar moveSave
      --putStrLn $ show $ intMove
      move <- return $ convertMove $ intMove
      putDebugStrLn hdl ("PLAY " `append` move)
      putStrLn $ "Calculated using search depth " ++ (show $ depth)
      ) (msDelay $ fromIntegral (time - G.aiTimeoutBuffer))   
    --

    -- manual play extension
    --move <- getLine
    --putDebugStrLn hdl $ pack $  "PLAY " ++ move
    
    getDebugLine hdl >>= parseStatic "+ OKTHINK"
    -- Play useless
    -- calculate move
    --  BIG TODO
    --
    getDebugLine hdl >>= parseStatic "+ MOVEOK"
    return True

gameOver :: Handle -> G.PlayerInfo -> (Maybe (Int,Text)) -> IO Bool
gameOver hdl player winner = do
    getPieceInfo hdl
    getDebugLine hdl >>= parseStatic "+ QUIT"
    
    case winner of
        Just (number, name) -> do
            putStrLn $ "Player #" ++ (show $ number) ++ " with name " ++ (unpack $ name) ++ " won the match"
            if number == G.pid player
                then putStrLn "YEAH, I WON"
                else putStrLn "Sadly I lost"
        Nothing             -> putStrLn $ "Gameover with draw"

    hClose hdl
    return False

-- | Does nothing. Implemented to catch only the timeout exception
timeoutHandler :: G.MorrisException -> IO ()
timeoutHandler G.TimeOutAI = return()               --putStrLn "Caught Timeout Exception"
timeoutHandler _           = throw G.AiException    -- should never be reached

-- | retrieve the stones from server
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

-- | read line from socket and if in debug mode, print this line
getDebugLine :: Handle -> IO Text
getDebugLine hdl = do
    line <- TextIO.hGetLine hdl
    when G.isDebug $ putStrLn $ "DEBUG: "++(show $ line)
    return line

-- | write line to socket and if in debug mode, print this line
putDebugStrLn :: Handle -> Text -> IO ()
putDebugStrLn hdl str = do
    TextIO.hPutStrLn hdl str
    when G.isDebug $ putStrLn $ "DEBUG: "++(unpack $ str)
    return ()
