{-# LANGUAGE DeriveGeneric #-}
{- fast implementation of a morris AI -}

module NineMorris.AI.Clever where

import Control.DeepSeq()
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
import Control.Exception hiding (mask,blocked)
import qualified Control.Parallel.Strategies as S
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout_par

newtype Board = Board Word64 deriving (Eq, Ord, Show)
type Mask = Word64

newtype Position = Position Int deriving (Eq, Ord, Show, Generic)

instance S.NFData Position

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

-- I am Black
data Player = Red | Black deriving (Eq, Ord, Show)

data Node = Node Board (Maybe Move) deriving (Eq, Ord, Show, Generic)

-- prefere take moves
instance Ord Move where
    compare FullMove{sndAction=a} FullMove{sndAction=b} 
        | (isNothing a) && (isJust b) = LT
        | otherwise                 = EQ

instance S.NFData  Node
instance Game_tree Node where
    is_terminal (Node board _) = 
        let
            player    = getBoardNextPlayer board
            handcount = getBoardHandCount player board
            pieces    = fromIntegral $ getNumPlayerPieces player board
            blocked   = fromIntegral $ blockedPiecesCnt player board
            pc        = pieces + handcount
        in pc < 3 || pc == blocked
    
    node_value (Node board _) =
        let
            player = getBoardNextPlayer board
            sig    = 1
        in sig * (round $ snd $ evalBoard player board)
        
    children (Node board _) =
        let
            moves = legalMoves board
            res = map (\move -> Node (playMove move board) (Just move)) moves
        in res


opponent :: Player -> Player
opponent Red = Black
opponent Black = Red

setBoardPosition :: Maybe Player -> Position -> Board -> Board
setBoardPosition posVal (Position posN) (Board rawBoard) =
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

-- Detect mills closed in last move

millClosedBitIdx :: Int
millClosedBitIdx = 47

setMillClosed :: Board -> Board
setMillClosed (Board rawBoard) = Board $ setBit rawBoard millClosedBitIdx

isMillClosed :: Board -> Bool
isMillClosed (Board rawBoard) = testBit rawBoard millClosedBitIdx

clearMillClosed :: Board -> Board
clearMillClosed (Board rawBoard) = Board $ clearBit rawBoard millClosedBitIdx

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

getBoardPlayer :: Board -> Player
getBoardPlayer b = opponent $ getBoardNextPlayer b
    
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

rawMillPos :: [[Int]]
rawMillPos = map (\n -> [n..2+n]) [0,3..23] ++ [
                    [0,9,21],[3,10,18],[6,11,15],
                    [1,4,7],[16,19,22],
                    [8,12,17],[5,13,20],[2,14,23]]
                    
millPositions :: [[Position]]
millPositions =
    map (map Position) $ rawMillPos

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

hideHandCount :: Mask
hideHandCount =  complement $ foldr (\pos -> (flip $ setBit) pos) 0 [48..63]

rawPlayerMask :: Mask
rawPlayerMask = (.&.) hideHandCount $ foldr (\pos -> (flip $ setBit) pos) 0 [0,2..49]

playerMask :: Player -> Mask
playerMask pl =
    case pl of
        Red   -> rawPlayerMask
        Black -> shiftL rawPlayerMask 1

millMasks :: [Mask]
millMasks =
    let positions = rawMillPos
    in map (\mill ->  foldr (\pos -> (flip $ setBit) (pos*2)) 0 mill) positions
    
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


getNumPlayerPieces :: Player -> Board -> Int
getNumPlayerPieces player (Board rawBoard) = popCount $ (.&.) rawBoard $ playerMask player
    

getPlayerMills' :: Player -> Board -> [[Position]]
getPlayerMills' player board =
    let list = filter (all (\p -> Just player == getBoardPosition p board)) millPositions
    in list --`S.using` S.rpar


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
        
getNumPlayerMills :: Player -> Board -> Int
getNumPlayerMills player board =
    let
        playerBoard = bordToMask player board
    in  foldl' (\acc mask -> if ((testMill playerBoard mask) == 3) then (acc+1) else acc) 0 millMasks

getCombinedBoardMask :: Board -> Mask
getCombinedBoardMask board@(Board raw) = 
    let
        opMask  = bordToMask Black board
    in (.|.) raw opMask

getFreePositions :: Board -> [Position]
getFreePositions board = 
    let
        combBoard = getCombinedBoardMask board
    in filter (\(Position p) -> not $ testBit combBoard (p*2)) allPositions

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

getNumPieceConf :: [[Position]] -> Int
getNumPieceConf dat = 
    let
        flat    = concat $ dat
        orgSize = length $ flat
        newSize = length $ nub flat
    in orgSize - newSize


-- | converts a Board into a mask shiftet so that the even bits belong to the given player
bordToMask :: Player -> Board -> Mask
bordToMask Red   (Board rawBoard) = rawBoard
bordToMask Black (Board rawBoard) = shiftR rawBoard 1
    
testMill :: Mask -> Mask -> Int
testMill rawBoard mask = popCount $ (.&.) rawBoard mask

playFirstAction :: Player -> FirstAction -> Board -> Board
playFirstAction player (Place p) board =
    setBoardPosition (Just player) p $
    reduceBoardHandCount player board
playFirstAction player (Move p p') board =
    setBoardPosition (Just player) p' $
    setBoardPosition Nothing p board

playSecondAction :: SecondAction -> Board -> Board
playSecondAction (Take pos) =
    (setBoardPosition Nothing pos).setMillClosed

playNext :: Board -> Board
playNext board =
    setBoardNextPlayer (opponent $ getBoardNextPlayer board) $
    clearMillClosed board

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

placeMoves' :: Board -> [FirstAction]
placeMoves' board = map Place $ filter (\p ->
    isNothing $ getBoardPosition p board) allPositions
    
placeMoves :: Board -> [FirstAction]
placeMoves board = map Place $ getFreePositions board

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

jmpMoves' :: Player -> Board -> [FirstAction]
jmpMoves' player board =
    concatMap (\p -> map (\p' -> Move p p') $
        filter (isNothing . flip getBoardPosition board) $
        filter (\possible -> isNothing $ getBoardPosition possible board) allPositions) $
    getPlayerPieces player board
    
jmpMoves :: Player -> Board -> [FirstAction]
jmpMoves player board =
    let
        myPos   = getPlayerPieces player board
    in concatMap (\p -> map (\p' -> Move p p') $ getFreePositions board) $ myPos
    
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

winValue :: Float
winValue = 1000.0

evalBoard :: Player -> Board -> (Maybe Player, Float)
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

        millClosed = if isMillClosed board then 1 else 0               -- (1) Mill closed in last move

    in case () of
           _ | pb < 3 || pb == bb -> (Just Red, winValue)
             | pa < 3 || pa == ba -> (Just Black, -winValue)
             | ha > 0    -> (Nothing, 18 * millClosed + 26 * (mca-mcb) + 1  * (bb-ba) + 6  * (pa-pb) + 12 * (twoa-twob) + 7 * (thra-thrb) + 1 * (fa-fb))
             | pa == 3   -> (Nothing, 40 * millClosed + 10 * (twoa-twob) + 1 * (thra-thrb))
             | otherwise -> (Nothing, 14 * millClosed + 43 * (mca-mcb) + 10 * (bb-ba) + 8 * (pa-pb) + 7 * (twoa-twob) + 42 * (dbma-dbmb))
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
        
        
aiMove :: Int -> Map Board Float -> Board -> Maybe Move
aiMove depth bias board =
    let
        (list,value) = principal_variation_search (Node board Nothing) (depth)
        (Node b m) = head $! drop 1 $! list
    in m `S.using` S.rseq
    
{-
aiMoveIterative :: Game_tree a => Int -> Map Board Float -> Board -> [a] -> (Maybe Move, [a])
aiMoveIterative depth bias board history =
    let
        (list,value) = head $ parallelize (principal_variation_search) (Node board Nothing) (depth)
        (Node b m) = head $! drop 1 $! list
    in (m,[]) `S.using` S.rseq
-}