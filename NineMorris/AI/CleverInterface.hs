{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.AI.CleverInterface
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a unified interface to the clever AI
-----------------------------------------------------------------------------
module NineMorris.AI.CleverInterface (
    convertMove,
    convertBoard,
    calculateIterativeMove )
where

import qualified NineMorris.AI.Clever as AI
import qualified NineMorris.Globals as G   
import Data.Text (Text,append)
import Control.Exception
import qualified Data.Map as Map
import Data.Maybe (isJust,fromMaybe)
import Control.Concurrent.MVar
import Control.Monad (when)

-- | convert a AI fullmove to the format the server expects
convertMove :: Maybe AI.Move -> Text
convertMove Nothing = throw G.AiException
convertMove (Just AI.FullMove { AI.fstAction=fsta, AI.sndAction=snda}) = 
    let
        partOne = convertFirstAction fsta
        partTwo = case snda of
                    Nothing     -> ""
                    (Just act)  -> ";" `append` convertSecondAction act
    in partOne `append` partTwo

convertFirstAction :: AI.FirstAction -> Text
convertFirstAction (AI.Place pos)     = (convertToServerPos $ pos)
convertFirstAction (AI.Move old new)  = (convertToServerPos $ old) `append` ":" `append` (convertToServerPos $ new)

convertSecondAction :: AI.SecondAction -> Text
convertSecondAction (AI.Take pos) = foldl (\res p -> res `append` ";" `append` convertToServerPos p) "" pos

convertToInternalPos :: Text -> AI.Position
convertToInternalPos pos =  AI.Position (Map.findWithDefault (-1) pos G.toAiPositions)

convertToServerPos :: AI.Position -> Text
convertToServerPos (AI.Position pos) = Map.findWithDefault "" pos G.toServerPositions

-- | convert the stone data recieved from server to AI board
convertBoard :: G.PlayerInfo    -- ^ PlayerInfo record of client player
             -> [G.StoneInfo]   -- ^ Parsed stone info records
             -> AI.Board        -- ^ result board
convertBoard player stones = AI.setBoardNextPlayer AI.Black $ foldl (convertSingleStone $ player) (AI.newBoard) stones

{- I am Black -}
convertSingleStone :: G.PlayerInfo -> AI.Board -> G.StoneInfo -> AI.Board
convertSingleStone (G.PlayerInfo {G.pid=pid}) (board) (G.StoneInfo {G.spid=playerId, G.sposition=pos})
    | pos == "A"       = board
    | pos == "C"       = if playerId == pid
                            then (AI.reduceBoardHandCount AI.Black) board
                            else (AI.reduceBoardHandCount AI.Red)   board
    | playerId == pid  = (AI.reduceBoardHandCount AI.Black) $ (AI.setBoardPosition (Just AI.Black) (convertToInternalPos pos) board)
    | otherwise        = (AI.reduceBoardHandCount AI.Red)   $ (AI.setBoardPosition (Just AI.Red)   (convertToInternalPos pos) board)

-- | Calculate a legal move with iterative deepening
calculateIterativeMove :: (MVar (Maybe AI.Move), MVar (Maybe AI.Move, Int)) -- ^ first Mvar hold the current best move, second the current calculation
                       -> AI.Board  -- ^ current board
                       -> Int       -- ^ current depth. Increased with each iteration
                       -> IO ()
calculateIterativeMove (moveStore,moveSave) board depth = do
    --putStrLn "calculateIterativeMove"
    m <- tryTakeMVar moveStore
    putStrLn $ "Current best: " ++ (show $ m)
    let realDepth = G.searchDepth + depth+2
    when (isJust m) $ do
        modifyMVar_ moveSave (\_ -> return $ (fromMaybe Nothing m, realDepth-1))
    if realDepth > G.maxSearchDepth
        then return () -- prevent explosion of search depth
        else do
            putMVar moveStore $! AI.aiMove realDepth Map.empty board
            calculateIterativeMove (moveStore,moveSave) board (depth+1)
