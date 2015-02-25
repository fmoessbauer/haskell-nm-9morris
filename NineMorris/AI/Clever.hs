-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.AI.Clever
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a (relatively) fast nine men morris AI. It can use
-- parallelization and should be conform with most of the rules defined
-- by the WMD (Weltmühlespiel Dachverband). The following rules are currently
-- not implemented:
--
-- no-capture moves if such move would lead to a instant win
-----------------------------------------------------------------------------

module NineMorris.AI.Clever (
    -- constructors
    Action(..),
    FirstAction(..),
    SecondAction(..),
    Move(..),
    Position(..),
    Board(..),
    Player(..),
    -- functions
    newBoard,
    setBoardNextPlayer,
    setBoardPosition,
    reduceBoardHandCount,
    aiMove
    )
where

import NineMorris.AI.Internal.Clever

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Parallel.Strategies as S
import Data.Tree.Game_tree.Negascout_par

-- | get new empty board
newBoard :: Board
newBoard =
    setBoardHandCount 9 Red $ setBoardHandCount 9 Black $ Board 0

aiMove' :: Int -> Map Board Float -> Board -> Maybe Move
aiMove' depth bias board =
    let moves = legalMoves board
        moveVals = zip moves $ S.parMap S.rpar (\m ->
            let board' = playMove m board
            in -evalTree board' (2*depth-1) (-1/0) (1/0) +
                   (fromMaybe 0 $ Map.lookup board' bias)) moves
        move = fst $ foldr (\(m,v) b@(bm,bv) ->
            if isNothing bm
            then (Just m,v)
            else (if v>bv then (Just m,v) else b)) (Nothing,-1/0) moveVals
        in move `S.using` S.rseq
        
        
-- | calculate the best legal move
aiMove :: Int               -- ^ search depth
       -> Map Board Float   -- ^ Map.empty, only to provide the same interface
       -> Board             -- ^ board for which the move will be calculated
       -> Maybe Move
aiMove depth _ board =
    let
        (list,_) = principal_variation_search (Node board Nothing) (depth)
        (Node _ m) = head $! drop 1 $! list
    in m  `S.using` S.rseq
    
{-
aiMoveIterative :: Game_tree a => Int -> Map Board Float -> Board -> [a] -> (Maybe Move, [a])
aiMoveIterative depth bias board history =
    let
        (list,value) = head $ parallelize (principal_variation_search) (Node board Nothing) (depth)
        (Node b m) = head $! drop 1 $! list
    in (m,[]) `S.using` S.rseq
-}