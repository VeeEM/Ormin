import Snake
import System.IO
import System.Console.Terminfo
import Data.Maybe

main :: IO ()
main = do
    terminal <- setupTermFromEnv
    let switchToAltBuffer = getCapability terminal (tiGetOutput1 "smcup")
    let switchToNormalBuffer = getCapability terminal (tiGetOutput1 "rmcup")
    case switchToAltBuffer of
        Just cap -> runTermOutput terminal cap
        Nothing -> return ()

    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    gameLoop snake (3,3) North 0
    case switchToNormalBuffer of
        Just cap -> runTermOutput terminal cap
        Nothing -> return ()
