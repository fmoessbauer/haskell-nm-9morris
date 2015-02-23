-----------------------------------------------------------------------------
-- 
-- Module      :  NineMorris
-- Copyright   :  (c) Felix Moessbauer
-- 
-- Maintainer  :  felix.moessbauer@campus.lmu.de
-- Stability   :  provisional
-- Portability :  portable
--
-- Main file of the Haskell-NM-9Morris Project
-----------------------------------------------------------------------------

import NineMorris.Client
import qualified NineMorris.Globals as G
import Paths_haskell_nm_9morris (version)
import Data.Version (showVersion)
import System.Environment (getArgs) -- <= für Command Line Arguments
import System.IO
import GHC.Conc (getNumCapabilities, getNumProcessors)

printHelp :: IO()
printHelp = putStrLn "Usage: <gameid> (<path to config File>) (<playerId 0|1 >)"

main :: IO()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn $ "Starting Client Version " ++ showVersion version

    -- determine CPU Info
    num_avail <- getNumProcessors
    num_used  <- getNumCapabilities
    putStrLn $ "Using " ++ show num_used ++ " of " ++ show num_avail ++ " cpu cores"
    putStrLn $ "AI Type: " ++ show G.aiType

    args <- getArgs
    case args of
        (gameid:path:player:_)  -> startClient gameid path player
        (gameid:path:_)         -> startClient gameid path G.defaultPlayer
        (gameid:_)              -> startClient gameid G.defaultConfig G.defaultPlayer
        _                       -> printHelp
    putStrLn "Stop Client"