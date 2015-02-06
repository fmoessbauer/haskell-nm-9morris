{-# LANGUAGE DeriveGeneric #-}
{- AI based on this implementation https://hackage.haskell.org/package/hsqml-demo-morris -}

module NineMorris.AI.Simple where

import Control.DeepSeq
import GHC.Generics
import Data.Word
import Data.Word.Odd
import Data.Bits
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
--import Debug.Trace
--import Numeric
import qualified NineMorris.Globals as G
import Control.Exception hiding (mask)
import qualified Control.Parallel.Strategies as S

newtype Board = Board Word64 deriving (Eq, Ord, Show)

newtype Position = Position Int deriving (Eq, Ord, Show, Generic)

instance NFData Position

data FirstAction
    = Place Position
    | Move Position Position
    deriving (Eq, Show)

newtype SecondAction
    = Take Position
    deriving (Eq, Show)

data Action
    = FirstAction FirstAction
    | SecondAction SecondAction
    deriving (Eq, Show)

data Move = FullMove {
    fstAction :: FirstAction,
    sndAction :: Maybe SecondAction}
    deriving (Eq, Show)

data Player = Red | Black deriving (Eq, Show)

-- prefere take moves
instance Ord Move where
    compare FullMove{sndAction=a} FullMove{sndAction=b} 
        | (isNothing a) && (isJust b) = LT
        | otherwise                 = EQ


opponent :: Player -> Player
opponent Red = Black
opponent Black = Red

setBoardPosition :: Maybe Player -> Position -> Board -> Board
setBoardPosition posVal (Position posN) (Board rawBoard) =
    -- Todo reduce handCount
    let encode Nothing      = 0
        encode (Just Red)   = 1
        encode (Just Black) = 2
        idx   = 2 * posN
        value = encode posVal `shiftL` idx
        mask  = complement $ shiftL 3 idx
    in Board $ (.|.) value $ (.&.) mask rawBoard 

getBoardPosition :: Position -> Board -> Maybe Player
getBoardPosition (Position posN) (Board rawBoard) =
    let decode 0 = Nothing
        decode 1 = Just Red
        decode 2 = Just Black
        decode _ = throw G.AiException
        idx = 2 * posN
    in decode $ (.&.) 3 $ shiftR rawBoard idx

handCountBitIdx :: Player -> Int
handCountBitIdx Red = 48
handCountBitIdx Black = 52

setBoardHandCount :: Word4 -> Player -> Board -> Board
setBoardHandCount count player (Board rawBoard) =
    let idx   = handCountBitIdx player
        value = fromIntegral count `shiftL` idx
        mask  = complement $ shiftL 0xF idx
    in Board $ (.|.) value $ (.&.) mask rawBoard

getBoardHandCount :: Player -> Board -> Word4
getBoardHandCount player (Board rawBoard) =
    let idx = handCountBitIdx player
    in fromIntegral $ (.&.) 0xF $ shiftR rawBoard idx

setBoardNextPlayer :: Player -> Board -> Board
setBoardNextPlayer player (Board rawBoard) =
    let adjBit Red = flip clearBit 63
        adjBit Black = flip setBit 63
    in Board $ adjBit player rawBoard

getBoardNextPlayer :: Board -> Player
getBoardNextPlayer (Board rawBoard) =
    let toPlayer False = Red
        toPlayer True = Black    
    in toPlayer $ testBit rawBoard 63

reduceBoardHandCount :: Player -> Board -> Board
reduceBoardHandCount pl board = 
    let 
        bc = getBoardHandCount pl board
    in setBoardHandCount (bc-1) pl board

newBoard :: Board
newBoard =
    setBoardHandCount 9 Red $ setBoardHandCount 9 Black $ Board 0

allPositions :: [Position]
allPositions =
    map Position [0..23]

millPositions :: [[Position]]
millPositions =
    map (map Position) $
        map (\n -> [n..2+n]) [0,3..23] ++ [
        [0,9,21],[3,10,18],[6,11,15],
        [1,4,7],[16,19,22],
        [8,12,17],[5,13,20],[2,14,23]]

{- prepared for better heuristic function
cornerPositions :: [Position]
cornerPositions =
    map Position [0,2,3,5,6,8,15,17,18,20,21,23]
-}

adjacencyMap :: Map Position [Position]
adjacencyMap =
    let ps = concatMap (\x@(k,v) -> [x,(v,k)]) $
            concatMap (\m -> zip m (tail m)) millPositions
    in foldr (\(k,v) m -> Map.insertWith' (++) k [v] m) Map.empty ps

{- prepared for better heuristic function
threePiecePosMap :: Map Position [Position]
threePiecePosMap =
    foldr (\p -> Map.insert p (fromJust $ Map.lookup p adjacencyMap)) Map.empty cornerPositions
-}

getPlayerPieces :: Player -> Board -> [Position]
getPlayerPieces player board =
    let list = filter (\p -> Just player == getBoardPosition p board) allPositions
    in list --`S.using` S.rpar

getPlayerMills :: Player -> Board -> [[Position]]
getPlayerMills player board =
    let list = filter (all (\p -> Just player == getBoardPosition p board)) millPositions
    in list --`S.using` S.rpar

playFirstAction :: Player -> FirstAction -> Board -> Board
playFirstAction player (Place p) board =
    setBoardPosition (Just player) p $
    setBoardHandCount (getBoardHandCount player board - 1) player board
playFirstAction player (Move p p') board =
    setBoardPosition (Just player) p' $
    setBoardPosition Nothing p board

playSecondAction :: SecondAction -> Board -> Board
playSecondAction (Take pos) =
    setBoardPosition Nothing pos

playNext :: Board -> Board
playNext board =
    setBoardNextPlayer (opponent $ getBoardNextPlayer board) board

playMove :: Move -> Board -> Board
playMove move board =
    let player = getBoardNextPlayer board
        pm (FullMove act1 Nothing) =
            playNext . playFirstAction player act1
        pm (FullMove act1 (Just act2)) =
            playNext . playSecondAction act2 . playFirstAction player act1
    in pm move board

partialToFullMoves :: Player -> Board -> FirstAction -> [Move]
partialToFullMoves player board act1 =
    let board' = playFirstAction player act1 board
        mills = getPlayerMills player board
        mills' = getPlayerMills player board'
        newMills = not $ null $ mills' \\ mills
        oPlayer = opponent player
        oPieces = getPlayerPieces oPlayer board
        oVulnerable = oPieces \\ (concat $ getPlayerMills oPlayer board)
        oTakeable = if null oVulnerable then oPieces else oVulnerable
        moves = (map (FullMove act1 . Just . Take) oTakeable) -- ++ [FullMove act1 Nothing] -- added No Take moves
    in if newMills && (not $ null oTakeable)
       then moves else [FullMove act1 Nothing]

placeMoves :: Board -> [FirstAction]
placeMoves board = map Place $ filter (\p ->
    isNothing $ getBoardPosition p board) allPositions

adjMoves :: Player -> Board -> [FirstAction]
adjMoves player board = 
    concatMap (\p -> map (\p' -> Move p p') $
        filter (isNothing . flip getBoardPosition board) $
        fromJust $ Map.lookup p adjacencyMap) $
    getPlayerPieces player board

blockedPiecesCnt :: Player -> Board -> Int
blockedPiecesCnt player board = 
    length $ filter (null) $
    map (\p -> 
        filter (isNothing . flip getBoardPosition board) $
        fromJust $ Map.lookup p adjacencyMap) $
    getPlayerPieces player board

jmpMoves :: Player -> Board -> [FirstAction]
jmpMoves player board =
    concatMap (\p -> map (\p' -> Move p p') $
        filter (isNothing . flip getBoardPosition board) $
        filter (\possible -> isNothing $ getBoardPosition possible board) allPositions) $
    getPlayerPieces player board

legalMoves :: Board -> [Move]
legalMoves board =
    let player  = getBoardNextPlayer board
        inHand  = getBoardHandCount player board > 0
        jmpAble = (length $ getPlayerPieces player board) == 3
        moves = if inHand
                then placeMoves board
                else if not $ jmpAble
                    then adjMoves player board
                    else jmpMoves player board
    in sort $ concatMap (partialToFullMoves player board) moves

winValue :: Float
winValue = 1000.0

evalBoard :: Player -> Board -> (Maybe Player, Float)
evalBoard player board =
    let 
        ha = fromIntegral (getBoardHandCount player board)
        hb = fromIntegral (getBoardHandCount player board)

        pa = ha  + fromIntegral (length $ getPlayerPieces player board)
        fa = fromIntegral $ length $ adjMoves player board
        ba = fromIntegral $ blockedPiecesCnt player board -- blocked A pieces
        ma = fromIntegral $ length $ getPlayerMills player board

        oPlayer = opponent player
        pb = hb + fromIntegral (length $ getPlayerPieces oPlayer board)
        fb = fromIntegral $ length $ adjMoves oPlayer board
        bb = fromIntegral $ blockedPiecesCnt oPlayer board -- blocked B pieces
        mb = fromIntegral $ length $ getPlayerMills oPlayer board

    in case () of
           _ | pb == bb || pb < 3 -> (Just Red, winValue)
             | pa == ba || pa < 3 -> (Just Black, -winValue)
             | ha > 0    -> (Nothing, 1.44*(ma-mb) + 0.50*(pa-pb) + 0.3*(fa-fb))
             | pa == 3   -> (Nothing, 2*(ma) + 2*(pa-pb))-- 1.00*(ma-mb) + 0.70*(pa-pb)) 
             | otherwise -> (Nothing, 1.34*(ma-mb) - 0.31*(ba-bb) + 0.34*(pa-pb)) -- + 0.3*(fa-fb)
             -- | otherwise -> (Nothing, 1.0*(pa-pb)+0.2*(fa-fb)+0.8*(ma-mb)) 
             -- heuristic based on https://kartikkukreja.wordpress.com/2014/03/17/heuristicevaluation-function-for-nine-mens-morris/
             -- Evaluation function for Phase 1 = 18 * (1) + 26 * (2) + 1 * (3) + 9 * (4) + 10 * (5) + 7 * (6)
             -- Evaluation function for Phase 2 = 14 * (1) + 43 * (2) + 10 * (3) + 11 * (4) + 8 * (7) + 1086 * (8)
             -- Evaluation function for Phase 3 = 16 * (1) + 10 * (5) + 1 * (6) + 1190 * (8)

evalTree :: Board -> Int -> Float -> Float -> Float
evalTree board depth alpha beta =
    let player = getBoardNextPlayer board
        (win, value) = evalBoard player board
        terminal = isJust win || depth == 0
        moves = legalMoves board
        next (m:ms) alpha' =
            let board' = playMove m board
                value' = -0.95 * evalTree board' (depth-1) (-beta) (-alpha')
            in if value' >= beta then value' else
                   if value' >= alpha' 
                   then next ms value' else next ms alpha'
        next [] alpha' = alpha'
    in if terminal then value else (next moves alpha)

aiMove :: Int -> Map Board Float -> Board -> Maybe Move
aiMove depth bias board =
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