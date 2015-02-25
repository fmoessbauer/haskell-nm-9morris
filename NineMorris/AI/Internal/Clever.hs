{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NineMorris.AI.Internal.Clever
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the internal AI functions. It is seperated to
-- unit test non exportet functions
-----------------------------------------------------------------------------
module NineMorris.AI.Internal.Clever (
    -- constructors
    Action(..),
    FirstAction(..),
    SecondAction(..),
    Move(..),
    Position(..),
    Board(..),
    Player(..),
    Node(..),
    -- constants
    handCountBitIdx,
    millClosedBitIdx,
    allPositions,
    rawMillPos,
    millPositions,
    adjacencyMap,
    -- board helper functions
    opponent,
    getBoardPosition,
    setBoardPosition,
    getBoardHandCount,
    setBoardHandCount,
    reduceBoardHandCount,
    getBoardNextPlayer,
    setBoardNextPlayer,
    setMillClosed,
    isMillClosed,
    clearMillClosed,
    getPlayerMills',
    getPlayerMills,
    testMill,
    getNumPlayerMills,
    hideHandCount,
    rawPlayerMask,
    playerMask,
    millMasks,
    getCombinedBoardMask,
    bordToMask,
    getPlayerPieces',
    getPlayerPieces,
    getNumPlayerPieces,
    getFreePositions,
    getTwoPieceConf,
    getNumPieceConf,
    blockedPiecesCnt,
    placeMoves',
    placeMoves,
    adjMoves,
    jmpMoves',
    jmpMoves,
    playFirstAction,
    playSecondAction,
    playNext,
    playMove,
    partialToFullMoves,
    legalMoves,
    evalBoard,
    evalTree
    ) 
where

import Control.DeepSeq()
import Control.Applicative ((<$>), (<*>)) -- only for permute2
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
import Control.Exception (throw)
import qualified Control.Parallel.Strategies as S
import Data.Tree.Game_tree.Game_tree

newtype Board = Board Word64 deriving (Eq, Ord, Show)
type Mask = Word64

newtype Position = Position Int deriving (Eq, Ord, Show, Generic)

instance S.NFData Position

data FirstAction
    = Place Position            -- ^ place a piece from the hand on the board
    | Move Position Position    -- ^ move the pieces from one pos to another (adjacent) pos
    deriving (Eq, Show)

newtype SecondAction
    = Take [Position]           -- ^ capture the pieces on this position
    deriving (Eq, Show)

data Action
    = FirstAction FirstAction
    | SecondAction SecondAction
    deriving (Eq, Show)

-- | game move consists of two actions
data Move = FullMove {
    fstAction :: FirstAction,
    sndAction :: Maybe SecondAction}
    deriving (Eq, Show)

-- | which players are possible. I am Black
data Player = Red | Black deriving (Eq, Ord, Show)

-- | node of the game tree consists of a board and a move that lead to the board
data Node = Node Board (Maybe Move) deriving (Eq, Ord, Show, Generic)


-- prefere take moves
instance Ord Move where
    compare FullMove{sndAction=a} FullMove{sndAction=b} 
        | (isNothing a) && (isJust b) = LT
        | otherwise                   = EQ

instance S.NFData  Node
instance Game_tree Node where
    is_terminal (Node board _) = loss $ getBoardNextPlayer board -- (loss $ Red) || (loss $ Black)
        where
            loss player = 
                let 
                    handcount = getBoardHandCount player board
                    pieces    = fromIntegral $ getNumPlayerPieces player board
                    blocked   = fromIntegral $ blockedPiecesCnt player board
                    pc        = pieces + handcount
                in pc < 3 || pc == blocked
    
    node_value (Node board _) =
        let
            player = getBoardNextPlayer board
        in round $ snd $ evalBoard player board
        
    children (Node board _) =
        let
            moves = legalMoves board
            res = map (\move -> Node (playMove move board) (Just move)) moves
        in res

{--------------------------------------------------------------------
  Constants
--------------------------------------------------------------------}

-- | At which bit in the board integer the handcount is stored (a 4 bit word)
handCountBitIdx :: Player -> Int
handCountBitIdx Red = 48
handCountBitIdx Black = 52

-- | which bit holds the mill closed flag
millClosedBitIdx :: Int
millClosedBitIdx = 47

-- | all board positions
allPositions :: [Position]
allPositions =
    map Position [0..23]

-- | all mill positions as integers
rawMillPos :: [[Int]]
rawMillPos = map (\n -> [n..2+n]) [0,3..23] ++ [
                    [0,9,21],[3,10,18],[6,11,15],
                    [1,4,7],[16,19,22],
                    [8,12,17],[5,13,20],[2,14,23]]

-- | all mill positions as positions
millPositions :: [[Position]]
millPositions =
    map (map Position) $ rawMillPos

{- prepared for better heuristic function
cornerPositions :: [Position]
cornerPositions =
    map Position [0,2,3,5,6,8,15,17,18,20,21,23]
-}

-- | Data.Map with a positon key and all adjacent positions as value
adjacencyMap :: Map Position [Position]
adjacencyMap =
    let ps = concatMap (\x@(k,v) -> [x,(v,k)]) $
            concatMap (\m -> zip m (tail m)) millPositions
    in foldr (\(k,v) m -> Map.insertWith' (++) k [v] m) Map.empty ps
    
{--------------------------------------------------------------------
  Board helper functions
--------------------------------------------------------------------}

-- | opponent of given player
opponent :: Player -> Player
opponent Red = Black
opponent Black = Red

-- | get the player or nothing on the given positon
getBoardPosition :: Position        -- ^ position to look up on the board
                 -> Board           -- ^ board
                 -> Maybe Player    -- ^ player or nothing on this position
getBoardPosition (Position posN) (Board rawBoard) =
    let decode 0 = Nothing
        decode 1 = Just Red
        decode 2 = Just Black
        decode _ = throw G.AiException
        idx = 2 * posN
    in decode $ (.&.) 3 $ shiftR rawBoard idx
    
-- | set the specified board position
setBoardPosition :: Maybe Player -- ^ Player of this position or Nothing if pos is free
                 -> Position     -- ^ Position
                 -> Board        -- ^ Board to set
                 -> Board
setBoardPosition posVal (Position posN) (Board rawBoard) =
    let encode Nothing      = 0
        encode (Just Red)   = 1
        encode (Just Black) = 2
        idx   = 2 * posN
        value = encode posVal `shiftL` idx
        mask  = complement $ shiftL 3 idx
    in Board $ (.|.) value $ (.&.) mask rawBoard 

-- | get the players hand count on the board
getBoardHandCount :: Player
                  -> Board
                  -> Word4  -- ^ handcount
getBoardHandCount player (Board rawBoard) =
    let idx = handCountBitIdx player
    in fromIntegral $ (.&.) 0xF $ shiftR rawBoard idx

-- | set the players hand count on the board
setBoardHandCount :: Word4  -- ^ handcount
                  -> Player -- ^ player
                  -> Board  -- ^ old board
                  -> Board  -- ^ new board
setBoardHandCount count player (Board rawBoard) =
    let idx   = handCountBitIdx player
        value = fromIntegral count `shiftL` idx
        mask  = complement $ shiftL 0xF idx
    in Board $ (.|.) value $ (.&.) mask rawBoard
    
-- | reduce the handcount of given player and board
reduceBoardHandCount :: Player -> Board -> Board
reduceBoardHandCount pl board = 
    let 
        bc = getBoardHandCount pl board
    in setBoardHandCount (bc-1) pl board

-- | get the next player on this board
getBoardNextPlayer :: Board -> Player
getBoardNextPlayer (Board rawBoard) =
    let toPlayer False = Red
        toPlayer True = Black    
    in toPlayer $ testBit rawBoard 63

-- | set the next player to given player
setBoardNextPlayer :: Player    -- ^ player to set
                   -> Board     -- ^ board to set player
                   -> Board
setBoardNextPlayer player (Board rawBoard) =
    let adjBit Red = flip clearBit 63
        adjBit Black = flip setBit 63
    in Board $ adjBit player rawBoard

-- | set that a mill was closed in the last move
setMillClosed :: Board -> Board
setMillClosed (Board rawBoard) = Board $ setBit rawBoard millClosedBitIdx

-- | test if mill was closed in the last move
isMillClosed :: Board -> Bool
isMillClosed (Board rawBoard) = testBit rawBoard millClosedBitIdx

-- | reset mill closed flag
clearMillClosed :: Board -> Board
clearMillClosed (Board rawBoard) = Board $ clearBit rawBoard millClosedBitIdx

-- | get the players mills via list operations
getPlayerMills' :: Player -> Board -> [[Position]]
getPlayerMills' player board =
    let list = filter (all (\p -> Just player == getBoardPosition p board)) millPositions
    in list --`S.using` S.rpar

-- | get the players mills via bit operations
getPlayerMills :: Player -> Board -> [[Position]]
getPlayerMills player board =
    let
        playerBoard = bordToMask player board
        logicResult = map (\mask -> (testMill playerBoard mask) == 3) millMasks
        --result      = filter (not.null) $ listToPosList 0 logicResult
        result      = snd $ foldl' (listToPosList) (0,[]) logicResult
    in result `S.using` S.rdeepseq
    where
        indexToPos :: Int -> [Position]
        indexToPos pos = millPositions !! pos
        
        listToPosList :: (Int,[[Position]]) -> Bool -> (Int,[[Position]])
        listToPosList (ind, acc) True  = (ind+1,(indexToPos ind) : acc)
        listToPosList (ind, acc) False = (ind+1,acc)
        {-
        listToPosList :: Int -> [Bool] -> [[Position]]
        listToPosList pos (True:xs)  = (indexToPos pos) : (listToPosList (pos+1) xs)
        listToPosList pos (False:xs) = listToPosList (pos+1) xs
        listToPosList _   []         = [[]]
        -}

-- | get the number of player mills via very fast bit operations
getNumPlayerMills :: Player -> Board -> Int
getNumPlayerMills player board =
    let
        playerBoard = bordToMask player board
    in  foldl' (\acc mask -> if ((testMill playerBoard mask) == 3) then (acc+1) else acc) 0 millMasks

{- prepared for better heuristic function
threePiecePosMap :: Map Position [Position]
threePiecePosMap =
    foldr (\p -> Map.insert p (fromJust $ Map.lookup p adjacencyMap)) Map.empty cornerPositions
-}

getPlayerPieces' :: Player -> Board -> [Position]
getPlayerPieces' player board =
    let list = filter (\p -> Just player == getBoardPosition p board) allPositions
    in list

getPlayerPieces :: Player -> Board -> [Position]
getPlayerPieces player (Board rawBoard) =
    let
        offset      = case player of
                           Red   -> 0
                           Black -> 1
        playerBoard = (.&.) rawBoard $ playerMask player
        result      = filter (\(Position p) -> testBit playerBoard (p*2+offset)) allPositions
    in result `S.using` S.rdeepseq


-- | get the number of given players pieces
getNumPlayerPieces :: Player -> Board -> Int
getNumPlayerPieces player (Board rawBoard) = popCount $ (.&.) rawBoard $ playerMask player

-- | get a list with free positions
getFreePositions :: Board -> [Position]
getFreePositions board = 
    let
        combBoard = getCombinedBoardMask board
    in filter (\(Position p) -> not $ testBit combBoard (p*2)) allPositions

-- | get a list with current two piece configurations of the given player.
-- | A 2-piece configuration is one to which adding one more piece would close a morris
getTwoPieceConf :: Player -> Board -> [[Position]]
getTwoPieceConf player board =
    let
        playerBoard = bordToMask player board
        combBoard   = getCombinedBoardMask board
    in  snd $ foldl' 
        (\(ind,acc) mask -> if (
            ((testMill playerBoard mask) == 2) && (not $ testMill combBoard mask == 3))
            then (ind+1,(testMask ind player board) : acc) else (ind+1,acc))
        (0,[]) millMasks
    where
        testMask :: Int -> Player -> Board -> [Position]
        testMask ind pl b
            | getNumPlayerPieces pl b == 3 || getBoardHandCount pl b > 0 = 
                filter (\p -> isJust $ getBoardPosition p board) $ millPositions !! ind
            | otherwise =
                let
                    curMill  = millPositions !! ind
                    free     = head $ filter (\p -> isNothing $ getBoardPosition p board) $ curMill
                    adj      = fromMaybe [] $ Map.lookup free adjacencyMap
                    possible = elem (Just pl) $ map (flip getBoardPosition board) $ adj  
                in if possible then delete free curMill else []

-- | get number of position lists, that share a common piece
--   that can be two 2-piece configurations, leading to a
--   3-piece conf, or two mills which are a double mill
getNumPieceConf :: [[Position]] -> Int
getNumPieceConf dat = 
    let
        flat    = concat $ dat
        orgSize = length $ flat
        newSize = length $ nub flat
    in orgSize - newSize
    
-- | get number of players pieces which can not move
blockedPiecesCnt :: Player -> Board -> Int
blockedPiecesCnt player board = 
    length $ filter (null) $
    map (\p -> 
        filter (isNothing . flip getBoardPosition board) $
        fromJust $ Map.lookup p adjacencyMap) $
    getPlayerPieces player board
    
-- | list with place actions
placeMoves' :: Board -> [FirstAction]
placeMoves' board = map Place $ filter (\p ->
    isNothing $ getBoardPosition p board) allPositions
    
-- | list with place actions implemented using bit operations
placeMoves :: Board -> [FirstAction]
placeMoves board = map Place $ getFreePositions board

-- | get all possible move actions
adjMoves :: Player -> Board -> [FirstAction]
adjMoves player board = 
    concatMap (\p -> map (\p' -> Move p p') $
        filter (isNothing . flip getBoardPosition board) $
        fromJust $ Map.lookup p adjacencyMap) $
    getPlayerPieces player board

-- | get all possible jump actions
jmpMoves' :: Player -> Board -> [FirstAction]
jmpMoves' player board =
    concatMap (\p -> map (\p' -> Move p p') $
        filter (isNothing . flip getBoardPosition board) $
        filter (\possible -> isNothing $ getBoardPosition possible board) allPositions) $
    getPlayerPieces player board
    
-- | get all possible jump actions via a propaply faster implementation
--   than 'jmpMoves''
jmpMoves :: Player -> Board -> [FirstAction]
jmpMoves player board =
    let
        myPos   = getPlayerPieces player board
    in concatMap (\p -> map (\p' -> Move p p') $ getFreePositions board) $ myPos

{--------------------------------------------------------------------
  operations with bitmasks
--------------------------------------------------------------------}

-- | test the board and the given mill mask.
--   count how many pieces are already in that potential mill
--   for performance reasons: to avoid double conversion board has to be converted to mask
testMill :: Mask -- ^ board as mask
         -> Mask -- ^ mill mask @see millMasks
         -> Int  -- ^ pieces in that mill
testMill rawBoard mask = popCount $ (.&.) rawBoard mask

-- | a mask that hides the handcount if anded with a board mask
--   all bits except the hand count bits are set to 1
hideHandCount :: Mask
hideHandCount =  complement $ foldr (\pos -> (flip $ setBit) pos) 0 [48..63]

-- | a mask that hides all opponent board bits
--   be carefull, this mask has to be shiftet to fit the correct player
--   see board layout in NineMorris.AI.Clever if not sure
rawPlayerMask :: Mask
rawPlayerMask = (.&.) hideHandCount $ foldr (\pos -> (flip $ setBit) pos) 0 [0,2..49]

-- | returns a mask that hides the opponent players bits
playerMask :: Player -> Mask
playerMask pl =
    case pl of
        Red   -> rawPlayerMask
        Black -> shiftL rawPlayerMask 1

-- | each mill as a board mask with only the mill bits set
millMasks :: [Mask]
millMasks =
    let positions = rawMillPos
    in map (\mill ->  foldr (\pos -> (flip $ setBit) (pos*2)) 0 mill) positions

-- | returns a mask where the bits of both players are set (or ed),
--   but at the same positions.
--   normally each second bit belongs to red respectively black.
--   In this case every second bit just shows if that position is
--   occupied
getCombinedBoardMask :: Board -> Mask
getCombinedBoardMask board@(Board raw) = 
    let
        opMask  = bordToMask Black board
    in (.|.) raw opMask

-- | converts a Board into a mask shiftet so that the even bits belong to the given player
bordToMask :: Player -> Board -> Mask
bordToMask Red   (Board rawBoard) = rawBoard
bordToMask Black (Board rawBoard) = shiftR rawBoard 1

{--------------------------------------------------------------------
  board modification / move operations
--------------------------------------------------------------------}

-- | apply the first move action to the board
playFirstAction :: Player       -- ^ player which turn will be applied
                -> FirstAction  -- ^ action to be applied
                -> Board        -- ^ old board
                -> Board        -- ^ result board
playFirstAction player (Place p) board =
    setBoardPosition (Just player) p $
    reduceBoardHandCount player board
playFirstAction player (Move p p') board =
    setBoardPosition (Just player) p' $
    setBoardPosition Nothing p board

-- | apply the second move action to the board
playSecondAction :: SecondAction -> Board -> Board
playSecondAction (Take pos) board =
    foldl' (\b p -> (setBoardPosition Nothing p).setMillClosed $ b) board $ pos

-- | set to opponent move
playNext :: Board -> Board
playNext board =
    setBoardNextPlayer (opponent $ getBoardNextPlayer board) $
    clearMillClosed board

-- | apply a full move to the board
playMove :: Move -> Board -> Board
playMove move board =
    let player = getBoardNextPlayer board
        pm (FullMove act1 Nothing) =
            playNext . playFirstAction player act1
        pm (FullMove act1 (Just act2)) =
            playNext . playSecondAction act2 . playFirstAction player act1
    in pm move board

-- | create full moves from partial moves.
--   if a piece should be captured, generate all moves
--   with possible capture positions
partialToFullMoves :: Player -> Board -> FirstAction -> [Move]
partialToFullMoves player board act1 =
    let board' = playFirstAction player act1 board
        mills = getPlayerMills player board
        mills' = getPlayerMills player board'
        newMills = mills' \\ mills
        oPlayer = opponent player
        oPieces = getPlayerPieces oPlayer board
        oVulnerable = oPieces \\ (concat $ getPlayerMills oPlayer board)
        oTakeable = if null oVulnerable then oPieces else oVulnerable
        moves = case () of
                     _ | (length $ newMills) == 2 && (length $ oTakeable) >= 2 -> (map (\pieces -> FullMove act1 (Just $ Take pieces )) (permute2 oTakeable))
                       | (length $ newMills) == 2 && (length $ oTakeable) == 1 -> (map (\pieces -> FullMove act1 (Just $ Take pieces )) (combine oTakeable (oPieces \\ oTakeable)))
                       | (not $ null $ newMills)  && (not $ null $ oTakeable)  -> (map (\piece  -> FullMove act1 (Just $ Take [piece])) oTakeable)
                       | otherwise -> [FullMove act1 Nothing]
            
    in moves
    where
        permute2 :: [a] -> [[a]]
        permute2 list = combine list list
        
        combine :: [a] -> [a] -> [[a]]
        combine l1 l2 = (\p p2 -> [p,p2]) <$> l1 <*> l2 -- crazy solution, but simple

    
-- | generate all possible legal moves
legalMoves :: Board -> [Move]
legalMoves board =
    let player  = getBoardNextPlayer board
        inHand  = getBoardHandCount player board > 0
        jmpAble = (getNumPlayerPieces player board) == 3
        moves = if inHand
                then placeMoves board
                else if not $ jmpAble
                    then adjMoves player board
                    else jmpMoves player board
    in sort $ concatMap (partialToFullMoves player board) moves

-- | heuristic function that rates the given board
evalBoard :: Player                 -- ^ player for that will be rated
          -> Board                  -- ^ board to rate
          -> (Maybe Player, Float)  -- ^ second value holds the value. The first is to be comatible with the functionalAi implementation
evalBoard player board =
    let 
        ha   = fromIntegral (getBoardHandCount player board)
        tPCA = getTwoPieceConf player board
        ma   = getPlayerMills player board

        pa   = ha  + (fromIntegral $ getNumPlayerPieces player board)  -- (4) number pieces
        fa   = fromIntegral $ length $ adjMoves player board           -- free adjacent positions
        ba   = fromIntegral $ blockedPiecesCnt player board            -- (3) blocked A pieces
        mca  = fromIntegral $ length ma                                -- (2) mills count
        twoa = fromIntegral $ length tPCA                              -- (5) two piece combinations
        thra = fromIntegral $ getNumPieceConf tPCA                     -- (6) three piece combinations
        dbma = fromIntegral $ getNumPieceConf ma                       -- (7) double morris

        oPlayer = opponent player

        hb   = fromIntegral (getBoardHandCount oPlayer board)
        tPCB = getTwoPieceConf oPlayer board
        mb   = getPlayerMills oPlayer board
        
        pb   = hb + (fromIntegral $ getNumPlayerPieces oPlayer board)  -- (4) number pieces
        fb   = fromIntegral $ length $ adjMoves oPlayer board          -- free adjacent positions
        bb   = fromIntegral $ blockedPiecesCnt oPlayer board           -- (3) blocked B pieces
        mcb  = fromIntegral $ length mb                                -- (2) mills count
        twob = fromIntegral $ length tPCB                              -- (5) two piece combinations
        thrb = fromIntegral $ getNumPieceConf tPCB                     -- (6) three piece combinations
        dbmb = fromIntegral $ getNumPieceConf mb                       -- (7) double morris

        sig  = case player of
                Black -> 1
                Red   -> -1
        millClosed = sig * if isMillClosed board then 1 else 0               -- (1) Mill closed in last move
        
        winConfa   = if pb < 3 || pb == bb then 1 else 0
        winConfb   = if pa < 3 || pa == ba then 1 else 0

    in case () of
           _ | ha > 0    -> (Nothing, 18 * millClosed + 26 * (mca-mcb) + 5  * (fa-fb) + 100  * (pa-pb) + 10 * (twoa-twob) + 7 * (thra-thrb))
             | pa == 3   -> (Nothing, 16 * millClosed + 10 * (twoa-twob) + 1 * (thra-thrb) + 1190 * (winConfb-winConfa))
             | otherwise -> (Nothing, 14 * millClosed + 43 * (mca-mcb) + 5 * (fa-fb) + 10 * (pa-pb) + 4 * (dbma-dbmb) + 1086 * (winConfb-winConfa))
             -- otherwise -> (Nothing, 14 * millClosed + 43 * (mca-mcb) + 20 * (twoa-twob) + 10 * (bb-ba) + 11 * (pa-pb) + 8 * (dbma-dbmb) + 1186 * (winConfb-winConfa))
             -- heuristic based on https://kartikkukreja.wordpress.com/2014/03/17/heuristicevaluation-function-for-nine-mens-morris/
             -- Evaluation function for Phase 1 = 18 * (1) + 26 * (2) + 1 * (3) + 9 * (4) + 10 * (5) + 7 * (6)
             -- Evaluation function for Phase 2 = 14 * (1) + 43 * (2) + 10 * (3) + 11 * (4) + 8 * (7) + 1086 * (8)
             -- Evaluation function for Phase 3 = 16 * (1) + 10 * (5) + 1 * (6) + 1190 * (8)

-- | evaluate a game tree. Not used anymore, just to prove that
--   the new game-tree algorithm calculates equal values
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