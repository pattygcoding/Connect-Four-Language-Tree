import Data.List (intersperse)

-- Constants
type Player = Char
empty :: Player
empty = ' '

player1 :: Player
player1 = 'X'

player2 :: Player
player2 = 'O'

rows :: Int
rows = 6

cols :: Int
cols = 7

-- Game state
type Board = [[Player]]

initialBoard :: Board
initialBoard = replicate rows (replicate cols empty)

-- Display the game board
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn " 1 2 3 4 5 6 7"
    mapM_ (putStrLn . ('|' :) . intersperse '|' . map (\p -> if p == empty then ' ' else p)) board
    putStrLn "---------------"

-- Drop a piece into a column
dropPiece :: Board -> Int -> Player -> Maybe Board
dropPiece board col player
    | col < 0 || col >= cols = Nothing
    | otherwise = case dropPiece' (reverse board !! col) player of
        Just newCol -> Just $ take col board ++ [newCol] ++ drop (col + 1) board
        Nothing -> Nothing

-- Helper function to drop a piece into a column
dropPiece' :: [Player] -> Player -> Maybe [Player]
dropPiece' [] _ = Nothing
dropPiece' (emptyPiece : rest) player = Just $ if emptyPiece == empty then player : rest else Nothing : rest
dropPiece' (piece : rest) player = (piece :) <$> dropPiece' rest player

-- Check for a win condition
checkWin :: Board -> Player -> Bool
checkWin board player =
    any (checkLine player) (rows board) ||
    any (checkLine player) (cols board) ||
    any (checkLine player) (diagonals board)

-- Check a line for a win
checkLine :: Player -> [Player] -> Bool
checkLine player line = go line 0
  where
    go [] count = count >= 4
    go (p : ps) count
        | p == player = go ps (count + 1)
        | otherwise = go ps 0

-- Get all diagonals in the board
diagonals :: Board -> [[Player]]
diagonals board = leftDiagonals ++ rightDiagonals
  where
    leftDiagonals = [ [ board !! (r + i) !! (c + i) | i <- [0 .. min (rows - r - 1) (cols - c - 1)] ] | r <- [0 .. rows - 1], c <- [0 .. cols - 1] ]
    rightDiagonals = [ [ board !! (r + i) !! (c - i) | i <- [0 .. min (rows - r - 1) (c - 0)] ] | r <- [0 .. rows - 1], c <- [cols - 1, cols - 2 .. 0] ]

-- Main game loop
playConnectFour :: Board -> Player -> IO ()
playConnectFour board player = do
    displayBoard board
    putStrLn $ "Player " ++ [player] ++ "'s turn. Enter column (1-7): "
    colStr <- getLine
    let col = read colStr :: Int
    case dropPiece board (col - 1) player of
        Just newBoard ->
            if checkWin newBoard player
                then do
                    displayBoard newBoard
                    putStrLn $ "Player " ++ [player] ++ " wins!"
                else if all (notElem empty) newBoard
                    then do
                        displayBoard newBoard
                        putStrLn "It's a draw! The board is full."
                    else playConnectFour newBoard (if player == player1 then player2 else player1)
        Nothing -> do
            putStrLn "Invalid column. Please try again."
            playConnectFour board player

-- Entry point
main :: IO ()
main = playConnectFour initialBoard player1
