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
import Data.Text (Text,pack,unpack,append,split)
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
    let (major,minor) = parseWelcome line
    
    -- verify that client and server versions match (major number has to be equal)
    when (not $ major ==  read (unpack $ head $ (split (=='.') G.internalVersion))) (
        putStrLn $ "WARNING: Client version '" ++ (unpack $ G.internalVersion) ++ "' does not match server version '"++ (show $ major) ++"."++ (show $ minor) ++"'.")

    -- send Client version
    putDebugStrLn hdl $ "VERSION " `append` G.internalVersion
    getDebugLine hdl >>= parseClientVersOk

    -- send Gameid
    putDebugStrLn hdl $ pack $ "ID "++gid

    -- recieve Gamekind or error
    getDebugLine hdl >>= (\str -> parseGamekindOk str gkind)

    -- recieve Game name
    gamename <- (getDebugLine hdl >>= (\str -> return $! parseGameName str))

    -- send player number
    let wishPid = case wishPlayer of
                   Nothing  -> ""
                   Just pid -> pack $ show $ pid
    putDebugStrLn hdl $ "PLAYER " `append` wishPid

    -- get player info
    mePlayer <- getDebugLine hdl >>= (\str -> return $! parseMePlayerInfo str) -- be carefull: use strict form!!

    total <- getDebugLine hdl >>= (\str -> return $! parseTotalPlayer str)
    
    when (not $ total == 2) (putStrLn $ "WARNING: This client is only made for a single opponent, not" ++ (show $ total-1))

    players <- replicateM (total-1) (getDebugLine hdl >>= (\str -> return $! parsePlayerInfo str))
    putStrLn $ show $ players

    -- recieve endplayers string
    getDebugLine hdl >>= parseEndplayers

    putStrLn $ "Playing game '" ++ (unpack $ gamename) ++ "' with " ++ (show $ total) ++ " players"
    putStrLn $ show $ mePlayer
    return $ mePlayer

-- | loop to handle the game phase.
handleGamePhase :: Handle -> G.PlayerInfo -> IO ()
handleGamePhase hdl player = do
    moveBuffer <- newEmptyMVar  -- buffer to store multi moves
    fix $ \loop -> do           -- prevents memory leaks
        line <- getDebugLine hdl
        ret <- case parseGPSwitch $ line of
            G.GP_WAIT          -> putDebugStrLn hdl "OKWAIT" >> return True
            G.GP_MOVE time     -> movePhase hdl player time moveBuffer
            G.GP_GAMEOVER dat  -> gameOver hdl player dat
        if ret then loop else return ()

movePhase :: Handle -> G.PlayerInfo -> Int -> (MVar [Text]) -> IO Bool
movePhase hdl player time buffer = do
    putStrLn $ "Begin MovePhase"
    putStrLn $ "Player:" ++ (show $ player)
    
    pieces <- getPieceInfo hdl

    let board = convertBoard player pieces
    putStrLn $ show $ board

    putDebugStrLn hdl "THINKING"
    
    -- calculate move or play second part of already calculated move
    empty <- isEmptyMVar buffer
    when (empty) (getAIMove time board buffer)
    playPartialMove hdl buffer

    -- manual play extension for debugging
    --move <- getLine
    --putDebugStrLn hdl $ pack $  "PLAY " ++ move
    
    getDebugLine hdl >>= parseStatic "+ OKTHINK"
    -- Blocking until timer fires
    
    getDebugLine hdl >>= parseStatic "+ MOVEOK"
    return True

-- | handle gameover game phase
gameOver :: Handle 
         -> G.PlayerInfo        -- player info of this player
         -> (Maybe (Int,Text))  -- nothing if draw, player number and name of winner otherwise
         -> IO Bool             -- always false to leave the game loop
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
    return False -- leave game loop

-- | Does nothing. Implemented to catch only the timeout exception
timeoutHandler :: G.MorrisException -> IO ()
timeoutHandler G.TimeOutAI = return()               --putStrLn "Caught Timeout Exception"
timeoutHandler _           = throw G.AiException    -- should never be reached

playPartialMove :: Handle -> (MVar [Text]) -> IO()
playPartialMove hdl buffer = do
    moves <- takeMVar buffer
    case moves of
         (x:xs) -> do
             putDebugStrLn hdl ("PLAY " `append` x)
             if xs == []
                then do
                    return ()
                else putMVar buffer xs
         []     -> throw G.AiException  -- the MVar has to be empty instead
    

getAIMove :: Int -> Board -> (MVar [Text]) -> IO()
getAIMove time board buffer = do
    moveStore <- newEmptyMVar
    moveSave  <- newMVar (Nothing, 0)
    --
    tid <- forkIO $ handle timeoutHandler $ calculateIterativeMove (moveStore,moveSave) board 0
    
    Timer.oneShotTimer (do
      throwTo tid G.TimeOutAI
      (intMove,depth) <- takeMVar moveSave
      --putStrLn $ show $ intMove
      move <- return $ convertMoveList $ intMove
      putMVar buffer move
      putStrLn $ "Calculated using search depth " ++ (show $ depth)
      ) (msDelay $ fromIntegral (time - G.aiTimeoutBuffer))   
    return ()
    
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
