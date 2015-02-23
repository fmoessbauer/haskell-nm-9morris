-- | Negascout and other (mostly alpha-beta pruning) algorithms for game-tree search
-- Copyright 2009 Colin Adams
--
-- This file is part of game-tree.
--
--  Game-tree is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  Game-tree is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with game-tree.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Tree.Game_tree.Negascout_par (
                                 -- * Access
                                 parallelize,
                                 negamax,
                                 alpha_beta_search,
                                 principal_variation_search,
                                 negascout
                 ) where

import Data.Tree.Game_tree.Game_tree
import qualified Control.Parallel.Strategies as S
import Control.Exception
import Data.Typeable
import Data.List
-- import Debug.Trace

data AlgorithmicException = Pattern_match_ex
    deriving (Show, Typeable)

instance Exception AlgorithmicException

{-# contract negascout :: Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- |  Negascout alpha-beta pruning algorithm.
--
-- Node_value needs to be sensitive to whose turn it is to move.
-- I.e. it must return values of the opposite sign if the other player is to move.
negascout :: Game_tree a => a   -- ^ State to be evaluated
          -> Int          -- ^ Search this deep
          -> ([a], Int)   -- ^ (Principal variation, Score)
negascout node depth = negascout'  ((minBound :: Int) + 1) (maxBound :: Int) node depth

{-# contract principal_variation_search :: Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Alpha-beta pruning with null-window search around every move after a move 
-- that improves alpha has been found
principal_variation_search :: (S.NFData a, Game_tree a) => a   -- ^ State to be evaluated
                           -> Int          -- ^ Search this deep
                           -> ([a], Int)   -- ^ (Principal variation, Score)
principal_variation_search node depth = principal_variation_search_par node depth False

principal_variation_search_par :: (S.NFData a, Game_tree a) => a   -- ^ State to be evaluated
                           -> Int          -- ^ Search this deep
                           -> Bool         -- ^ Parallelize this stage
                           -> ([a], Int)   -- ^ (Principal variation, Score)
principal_variation_search_par node depth parallel
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      = case pvs ((minBound :: Int) + 1) (maxBound :: Int) (children node) depth parallel of
                                         (pvm, pvv) -> (node:pvm, pvv)
{-# contract alpha_beta_search ::  Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Normal alpha beta pruning (no window).
alpha_beta_search ::(S.NFData a, Game_tree a) => a -- ^ State to be evaluated
           -> Int               -- ^ Search this deep
           -> ([a], Int)        -- ^ (Principal variation, Score)
alpha_beta_search node depth =
    alpha_beta ((minBound :: Int) + 1) (maxBound :: Int) node depth False

{-# contract negamax :: Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Plain negamax (= minimax with negative scores at alternate levels).
--  No alpha-beta pruning.
negamax :: Game_tree a => a  -- ^ State to be evaluated
        -> Int         -- ^ Search this deep
        -> ([a], Int)  -- ^ (Principal variation, Score)
negamax node depth
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      =  case children node of
                                          (c:cs) -> (node:pvm, pvv)
                                              where (pvm, pvv) = negaLevel (neg (negamax c (depth - 1))) cs
                                          _      -> throw Pattern_match_ex
    where negaLevel prev_best@(_, old_v) (n:nn) =
              negaLevel best4 nn
                  where best4 = case neg $ negamax n (depth - 1) of 
                                  value@(_, v) | v > old_v -> value
                                               | otherwise -> prev_best
          negaLevel best _ = best                                 
          neg (m, v) = (m, -v)

-- Implementation

{-# contract negascout' :: Ok -> Ok -> Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- |  Negascout alpha-beta pruning algorithm.
negascout' :: Game_tree a => Int -- ^ Minimum score maximising player is assured
          -> Int           -- ^ Maximum score minimizing player is assured
          -> a             -- ^ State to be evaluated
          -> Int           -- ^ Search this deep
          -> ([a], Int)    -- ^ (Principal variation, Score)
negascout'  alpha beta node depth
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      = let (pvm, pvv) = negascout'' [] alpha beta $ children node
                                       in (node:pvm, pvv)
    where
      d = depth - 1
      negascout'' npv nalpha _ [] = (npv, nalpha)
      negascout'' npv nalpha b (c:cs) = result
          where (n', alpha') = let (new_n', new_alpha') = negascout' (-b) (-nalpha) c d
                               in if (-new_alpha') > nalpha
                                  then (new_n', -new_alpha')
                                  else (npv, nalpha)
                result
                    | alpha' >= beta = ((c:n'), alpha')                                 -- beta cut-off
                    | alpha' >= b    = result'                                          -- check if null-window failed high
                    | otherwise      = negascout'' n' alpha' (alpha' + 1) cs            -- new null-window
                    where
                      result'
    -- CAUTION! Tracing shows the next line is not exercised by the unit tests.
                          | alpha'' >= beta = ((c:n''), alpha'')                        -- beta cut-off
                          | otherwise       = negascout'' n'' alpha'' (alpha'' + 1) cs
                          where
                            alpha'' = - alpha'''
                            (n'', alpha''') = negascout' (-beta) (-alpha') c d          -- full re-search

{-# contract pvs :: Ok -> Ok -> NotNull -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Principle variation search internals (soft fail).
--
-- The search continues as long as alpha < pvs < beta.
-- As soon pvs hits one these bounds the search stops and returns best.
pvs :: (S.NFData a, Game_tree a) => Int -> Int -> [a] -> Int -> Bool -> ([a], Int)
pvs alpha beta (c:cs) depth p = case negpvs (-beta) (-alpha) c d of
                                best -> negaLevel best alpha beta cs
    where d = depth - 1
          negaLevel prev_best@(_, old_v) prev_alpha beta' (n:nn) | old_v < beta'
            = negaLevel best4 alpha' beta' nn
                where best4 = case negpvs (-alpha' - 1) (-alpha') n d of 
                                 value@(_, v) | (alpha' < v) && (v < beta')
                                                  -> negpvs (-beta') (-v) n d -- re-search
                                              | (v > old_v) -> value
                                              | otherwise -> prev_best 
                      alpha' = if old_v > prev_alpha then old_v else prev_alpha
          negaLevel best _ _ _     = best                                 
          negpvs alpha'' beta'' node d'
              | is_terminal node || d' == 0 = ([node], - (node_value node))
              | otherwise = let
                                nodes  = children node
                                result = if (length $ nodes) < 10 && d' > 1 && (not $ p)
                                            then parallelize (principal_variation_search_par) node d'
                                            else case nodes of
                                                    nn' -> (node:pvm, -pvv)
                                                        where (pvm, pvv) = pvs alpha'' beta'' nn' d' p
                            in result
pvs _ _ _ _ _ = throw Pattern_match_ex
{-# contract alpha_beta :: Ok -> Ok -> Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Normal alpha beta pruning (no window).
alpha_beta :: (S.NFData a, Game_tree a) => Int -- ^ Minimum score maximising player is assured
           -> Int          -- ^ Maximum score minimizing player is assured
           -> a            -- ^ State to be evaluated
           -> Int          -- ^ Search this deep
           -> Bool         -- ^ Parallelize this stage
           -> ([a], Int)   -- ^ (Principal variation, Score)
alpha_beta alpha beta node depth p
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      =  let
                                            nodes  = children node
                                            search = (alpha_beta ((minBound :: Int) + 1) (maxBound :: Int))
                                            result = if (length $ nodes) < 10 && (not $ p)
                                                        then parallelize (search) node depth
                                                        else (node:pvm, pvv)
                                                            where (pvm, pvv) = negaLevel ([], (minBound :: Int) + 2) alpha beta nodes
                                        in result
    where negaLevel prev_best@(_, old_v) prev_alpha beta' (n:nn)
            | old_v < beta' = negaLevel best4 alpha' beta' nn
                where best4 = case neg $ alpha_beta (-beta') (-alpha') n (depth - 1) p of 
                                 value@(_, v) | (v > old_v) -> value
                                              | otherwise -> prev_best
                      alpha' = if old_v > prev_alpha then old_v else prev_alpha
          negaLevel best _ _ _     = best                                 
          neg (m, v) = (m, -v)
        

-- | parallelize a search algorithm by parallel evaluation of branches.
parallelize ::(S.NFData a, Game_tree a) => 
              (a -> Int -> Bool -> ([a], Int))  -- ^ search function to parallelize
           -> a                                 -- ^ State to be evaluated
           -> Int                               -- ^ Search this deep
           -> ([a], Int)                        -- ^ (Principal variation, Score) ordered by move value
parallelize f node depth =
    let
        nodes = children node
        res   = S.parMap S.rdeepseq (\n -> (f n (depth-1)) True) $ nodes
        first = ([node],minBound)
        best  = inject node $! (foldl' (getBest) first res)
        --set   = sortBy (comp) $ map (\n -> inject node $ invert n) res
    in if is_terminal node
          then first
          else best
    where
        getBest :: Game_tree a => ([a], Int) -> ([a], Int) -> ([a], Int)
        getBest (bestn, bestv) (n, v) = 
            if -v > bestv    -- v has to be inverted, because level 1 is a minimising level
               then (n, -v)
               else (bestn, bestv)
        inject :: Game_tree a => a -> ([a], Int) -> ([a], Int)
        inject a (n,v) = (a:n,v)
        
        -- comp :: ([a], Int) -> ([a], Int) -> Ordering
        -- comp (_,a) (_,b) = compare a b
