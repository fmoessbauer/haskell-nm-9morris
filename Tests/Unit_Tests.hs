-- | Unit testing file for the NineMorris AI
--   plugged to the cabal test suite, connector adapted from
--   https://robertmassaioli.wordpress.com/2013/12/02/connecting-haskell-hunit-tests-to-cabal-testsuite/
--
--   this test module can be loaded in GHCi and run via main
-----------------------------------------------------------------
module Tests.Unit_Tests where

-- --------------------------------------------------------------
-- ---------------- Just skip this block ------------------------
-- --------------------------------------------------------------

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit as HU
-- -----
import Test.HUnit
import NineMorris.AI.Clever
import NineMorris.AI.Internal.Clever
import Control.Applicative
 
hunitTests = allTests
 
runHUnitTests :: HU.Test -> IO TS.Progress
runHUnitTests tests = do
   (HU.Counts cases tried errors failures) <- HU.runTestTT tests
   return $ if errors > 0
      then TS.Finished $ TS.Error "There were errors in the HUnit tests"
      else if failures > 0
         then TS.Finished $ TS.Fail "There were failures in the HUnit tests"
         else TS.Finished TS.Pass
 
tests :: IO [TS.Test]
tests = return [ TS.Test hunit ]
  where
    hunit = TS.TestInstance
        { TS.run = runHUnitTests hunitTests
        , TS.name = "HUnit Test Cases"
        , TS.tags = ["hunit"]
        , TS.options = []
        , TS.setOption = \_ _ -> Right hunit
        }

-- --------------------------------------------------------------
-- ---------------- real testing logic --------------------------
-- --------------------------------------------------------------

-- Micro Unit tests
-- Board tests

simple          = Board 9256586702582972480
s_mills_red     = [map Position $ [3,10,18]]
s_mills_black   = []
s_free_pos      = map Position $ [0,1,2,4,5,6,7,8,9,11,12,13,14,15,16,17,20,21,22,23]
s_pieces_red    = map Position $ [3,10,18]
s_pieces_black  = map Position $ [19]

complex = Board 9223378926358251178
c_mills_red     = []
c_mills_black   = [map Position $ [0,1,2]]
c_free_pos      = map Position $ [6,8,15,16,18,22,23]
c_pieces_red    = map Position $ [5,7,11,13,14,17,19,21]
c_pieces_black  = map Position $ [0,1,2,3,4,9,10,12,20]

randomBoards    = [Board 9237516223792547242,
                   Board 9266156233314795524,
                   Board 9261371708466790404,
                   Board 9256586084107683208,
                   Board 9237455131683094869,
                   Board 9247025280881787157,
                   Board 9266156233314795776,
                   Board 9261652908565331976]

testsSimple = TestList [
            TestCase (assertEqual "num pieces black" 1 (getNumPlayerPieces Black simple)),
            TestCase (assertEqual "num pieces red"   3 (getNumPlayerPieces Red   simple)),
                
            TestCase (assertEqual "pieces black" s_pieces_black (getPlayerPieces Black simple)),
            TestCase (assertEqual "pieces red"   s_pieces_red   (getPlayerPieces Red   simple)),
            
            TestCase (assertEqual "morris count black" 0 (getNumPlayerMills  Black simple)),
            TestCase (assertEqual "morris count red"   1 (getNumPlayerMills  Red   simple)),
                
            TestCase (assertEqual "mills black" s_mills_black (getPlayerMills Black simple)),
            TestCase (assertEqual "mills red"   s_mills_red   (getPlayerMills Red   simple)),
            
            TestCase (assertEqual "two piece conf black" 0 (length $ getTwoPieceConf Black simple)),
            TestCase (assertEqual "two piece conf red"   0 (length $ getTwoPieceConf Red simple)),
            
            TestCase (assertEqual "three piece conf black" 0 (getNumPieceConf $ getTwoPieceConf Black simple)),
            TestCase (assertEqual "three piece conf red"   0 (getNumPieceConf $ getTwoPieceConf Red simple)),
            
            TestCase (assertEqual "double morris black" 0 (getNumPieceConf $ getPlayerMills Black simple)),
            TestCase (assertEqual "double morris red"   0 (getNumPieceConf $ getPlayerMills Red simple)),
            
            TestCase (assertEqual "blocked pieces black" 0 (blockedPiecesCnt Black simple)),
            TestCase (assertEqual "blocked pieces red"   1 (blockedPiecesCnt Red   simple)),
            
            TestCase (assertEqual "free positions" s_free_pos (getFreePositions simple))
        ]
        
testsComplex = TestList [
            TestCase (assertEqual "num pieces black" 9 (getNumPlayerPieces Black complex)),
            TestCase (assertEqual "num pieces red"   8 (getNumPlayerPieces Red   complex)),
                
            TestCase (assertEqual "pieces black" c_pieces_black (getPlayerPieces Black complex)),
            TestCase (assertEqual "pieces red"   c_pieces_red   (getPlayerPieces Red   complex)),
            
            TestCase (assertEqual "morris count black" 1 (getNumPlayerMills  Black complex)),
            TestCase (assertEqual "morris count red"   0 (getNumPlayerMills  Red   complex)),
                
            TestCase (assertEqual "mills black" c_mills_black (getPlayerMills Black complex)),
            TestCase (assertEqual "mills red"   c_mills_red   (getPlayerMills Red   complex)),
            
            TestCase (assertEqual "two piece conf black" 1 (length $ getTwoPieceConf Black complex)),
            TestCase (assertEqual "two piece conf red"   0 (length $ getTwoPieceConf Red complex)),
            
            TestCase (assertEqual "three piece conf black" 0 (getNumPieceConf $ getTwoPieceConf Black complex)),
            TestCase (assertEqual "three piece conf red"   0 (getNumPieceConf $ getTwoPieceConf Red complex)),
            
            TestCase (assertEqual "double morris black" 0 (getNumPieceConf $ getPlayerMills Black complex)),
            TestCase (assertEqual "double morris red"   0 (getNumPieceConf $ getPlayerMills Red complex)),
            
            TestCase (assertEqual "blocked pieces black" 7 (blockedPiecesCnt Black complex)),
            TestCase (assertEqual "blocked pieces red"   2 (blockedPiecesCnt Red   complex)),
            
            TestCase (assertEqual "free positions" c_free_pos (getFreePositions complex))
        ]
      
testsEqualImplementation b p= TestList [
            TestCase (assertEqual ("getPlayerMills "++(show $ p))       (getPlayerMills' p b)           (getPlayerMills p b)),
            TestCase (assertEqual ("getPlayerPieces "++(show $ p))      (getPlayerPieces' p b)          (getPlayerPieces p b)),
            TestCase (assertEqual ("placeMoves "++(show $ p))           (placeMoves' b)                 (placeMoves b)),
            TestCase (assertEqual ("jmpMoves "++(show $ p))             (jmpMoves' p b)                 (jmpMoves p b)),
            TestCase (assertEqual ("getNumPlayerPieces "++(show $ p))   (length $ getPlayerPieces p b)  (getNumPlayerPieces p b))
        ]

appliedTestCases = TestList $ (\b p -> testsEqualImplementation b p) <$> randomBoards <*> [Red,Black]

allTests = TestList[
        TestLabel "simple Board"         testsSimple,
        TestLabel "complex Board"        testsComplex,
        TestLabel "equal Implementation" appliedTestCases
    ]
        
main = runTestTT $ allTests