module Snake where

import Control.Monad
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import System.IO

data Direction = North | South | West | East deriving (Eq, Show)

type Snake = [(Int, Int)]
type Apple = (Int, Int)
snake :: [(Int, Int)]
snake = [(5,5), (5,6), (5,7), (5,8), (5,9)]

gameWidth = 15
gameHeight = 15

tickDelay = 125000

field = take gameHeight $ cycle $ [replicate gameWidth 'a']

addGap :: String -> String
addGap s = foldr (\a b -> ' ' : a : b) [] s

printField2 :: Snake -> Apple -> String
printField2 snake apple = addGap $ getTileChars 0 0
    where
        getTileChars :: Int -> Int -> String
        getTileChars x y = getTileChar (x,y) snake apple : 
            if ((y + 1) >= gameHeight && endOfRow x)
             then []
             else if endOfRow x
              then getTileChars 0 (y+1)
              else getTileChars (x + 1) y

getTileChar :: (Int, Int) -> Snake -> Apple -> Char
getTileChar tile snake apple = if endOfRow $ fst tile
                                then '\n'
                                else if tileInSnake snake tile
                                 then 'x'
                                 else if apple == tile
                                  then 'o'
                                  else '.'
                                 

endOfRow :: Int -> Bool
endOfRow x = x >= gameWidth

tileInSnake :: Snake -> (Int, Int) -> Bool
tileInSnake snake tile = elem tile snake


printField :: [String] -> String
printField field = foldr (\a b -> a ++ "\n" ++ b) [] field

generateField :: [(Int, Int)] -> (Int, Int) -> String
generateField snake apple =  undefined

--makeRows :: Int -> Int -> Snake -> String
--makeRows width height snake = take width $ makeRow 

makeRow :: Int -> [(Int, Int)] -> String
makeRow row snake = makeRow' 0
    where
        makeRow' column = case (elem (column,row) snake) of
            True -> 'x' : makeRow' (column + 1)
            False -> ' ': makeRow' (column + 1)


moveHead :: (Int, Int) -> Direction -> (Int, Int)
moveHead (x, y) d = case d of
    North   -> if y > 0 then (x, (y-1)) else (x, gameHeight - 1)
    South   -> if y < (gameHeight - 1) then (x, (y+1)) else (x, 0)
    West    -> if x > 0 then ((x-1), y) else (gameWidth - 1, y)
    East    -> if x < (gameWidth - 1) then ((x+1),y) else (0, y)

eatsApple :: Snake -> Apple -> Bool
eatsApple snake apple = head snake == apple

handleApple :: Snake -> Apple -> Bool -> IO Apple
handleApple _ apple False = return apple
handleApple snake apple True = handleApple' apple
    where
        handleApple' :: Apple -> IO Apple
        handleApple' apple = do
            newX <- (randomRIO (0, gameWidth - 1))
            newY <- (randomRIO (0, gameHeight - 1))
            let newApple = (newX, newY)
            if tileInSnake snake newApple
                then handleApple' newApple
                else return newApple

moveSnake :: Snake -> Direction -> Bool -> Snake
moveSnake snake direction extend =
    if extend
        then
            moveHead (head snake) direction : snake
        else
            moveHead (head snake) direction : init snake

clear :: IO ()
clear = putStr "\ESC[2J"

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- hGetChar stdin
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getKeyIfPressed :: IO (Maybe [Char])
getKeyIfPressed = do
    keyPressed <- hReady stdin
    if keyPressed
        then Just <$> getKey
        else return Nothing

getDirection :: Maybe [Char] -> Direction -> IO Direction
getDirection key oldDirection =
    case key of
        Nothing -> return oldDirection
        Just k ->
            case (drop ((length k) - 3) k) of
                "\ESC[A" -> return North
                "\ESC[B" -> return South
                "\ESC[C" -> return East
                "\ESC[D" -> return West
                otherwise -> return oldDirection

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East

correctDirection :: Direction -> Direction -> Direction
correctDirection newDirection oldDirection =
    if newDirection == oppositeDirection oldDirection
        then oldDirection
        else newDirection

updateScore :: Int -> Bool -> Int
updateScore score True = score + 1
updateScore score False = score

gameLoop :: Snake -> Apple -> Direction -> Int -> IO ()
gameLoop snake point oldDirection score = do
    key <- getKeyIfPressed
    direction <- (correctDirection <$> getDirection key oldDirection <*> return oldDirection)
    let eats = eatsApple snake point
    let newScore = updateScore score eats
    let newSnake = moveSnake snake direction eats
    newPoint <- handleApple snake point eats
    let isGameOver = tileInSnake (tail snake) (head snake)
    threadDelay tickDelay
    --threadDelay 1000000

    clear
    putStr $ printField2 newSnake point
    putStrLn $ "Score: " ++ show newScore
    putStrLn $ show direction
    pressed <- hReady stdin
    putStrLn $ show pressed
    if key == Just "q"
        then return ()
        else
            if isGameOver
                then gameOver newScore
                else gameLoop newSnake newPoint direction newScore

gameOver :: Int -> IO ()
gameOver score = do
    threadDelay tickDelay
    putStrLn "Game over!"
    putStrLn $ "Score: " ++ show score
    clear
    key <- getKeyIfPressed
    case key of
        Just " " -> do
            apple <- handleApple snake (0,0) True
            gameLoop snake apple North 0
        Just "q" -> return ()
        otherwise -> gameOver score
