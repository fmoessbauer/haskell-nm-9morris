{-
Main file of the Haskell-NM-9Morris Project
-}
import NineMorris.Client
import qualified NineMorris.Globals as G
import Paths_haskell_nm_9morris (version)
import Data.Version (showVersion)
import System.Environment (getArgs) -- <= für Command Line Arguments
import System.IO

printHelp :: IO()
printHelp = putStrLn "Usage: <gameid> (<path to config File>)"

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Starting Client Version " ++ showVersion version
    args <- getArgs
    case args of
        (gameid:path:_) -> startClient gameid path
        (gameid:_)      -> startClient gameid G.defaultConfig
        _                -> printHelp
    putStrLn "Stop Client"