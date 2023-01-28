module Main where

import qualified Data.Map as M

main :: IO ()
main = do
    renderGame initialGame

    gameLoop initialGame

    putStrLn "Game over, come again soon."

gameLoop :: Game -> IO ()
gameLoop game = do
    nextGame <- readAndPerformMove game

    -- Render
    renderGame nextGame

    -- Game over condition
    let outcome = checkGameOutcome nextGame

    case outcome of
        Nothing -> gameLoop nextGame
        Just WinX -> putStrLn "Yay X you won!"
        Just WinO -> putStrLn "Yay O you won!"
        Just Draw -> putStrLn "Its a draw!"

readAndPerformMove :: Game -> IO Game
readAndPerformMove game = do
    move <- readMove
    case performMove move game of
        Nothing -> do
            putStrLn "Field is already occupied!"
            readAndPerformMove game
        Just nextGame -> pure nextGame

initialGame :: Game
initialGame = []

readMove :: IO Move
readMove = do
    row <- readLn
    col <- readLn

    case parseMove (row, col) of
        Nothing -> putStrLn "Invalid move!" >> readMove
        Just move -> pure move
  where
    parseMove :: (Int, Int) -> Maybe Move
    parseMove move =
        if move `elem` [(r', c') | r' <- [0 .. 2], c' <- [0 .. 2]]
            then Just move
            else Nothing

performMove :: Move -> Game -> Maybe Game
performMove move game
    | move `elem` game = Nothing
    | otherwise = Just (game ++ [move])

getBoard :: Game -> Board
getBoard moves = M.fromList $ zip moves $ cycle [X, O]

renderGame :: Game -> IO ()
renderGame game = do
    renderRow 0
    renderRow 1
    renderRow 2
  where
    board = getBoard game

    renderRow :: Int -> IO ()
    renderRow r = do
        -- :: [Int] -> ((Int) -> IO()) -> IO ()
        mapM_ (curry renderField r) [0..2]
        putStrLn ""

    renderField :: (Int, Int) -> IO ()
    renderField pos = putStr $ maybe "-" show $ M.lookup pos board

-- winx wino draw none
checkGameOutcome :: Game -> Maybe Outcome
checkGameOutcome game = Nothing -- TODO

type Board = M.Map Move Symbol

data Symbol = X | O
    deriving (Show)

-- list of 9 things, map (int,int)->thing, list moves
type Game = [Move]

-- we want to limit moves to 0..3
data Row = R1 | R2 | R3
data Col = C1 | C2 | C3

-- type Move = (Row, Col)
-- data Move = Move Row Col
type Move = (Int, Int)

data Outcome = WinX | WinO | Draw

-- Board state; type, function; type player