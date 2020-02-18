import Snake
import System.IO
import System.Console.Terminfo
import Data.Maybe
{-
main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
-}

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
