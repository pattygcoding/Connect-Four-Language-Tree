import Data.List (transpose)

type Board = [[Char]]
type Column = Int
type Player = Char

rows, columns :: Int
rows = 6
columns = 7

player1, player2, empty :: Player
player1 = 'X'
player2 = 'O'
empty = ' '

main :: IO ()
main = playGame (replicate rows (replicate columns empty)) player1

playGame :: Board -> Player -> IO ()
playGame board player = do
    putStrLn $ displayBoard board
    putStrLn $ "Player " ++ [player] ++ "'s turn. Enter column (1-7):"
    col <- getLine
    let column = read col - 1
    if column < 0 || column >= columns || not (isValidMove board column) then do
        putStrLn "Invalid move. Try again."
        playGame board player
    else do
        let newBoard = dropToken board column player
        if checkWin newBoard player then do
            putStrLn $ displayBoard newBoard
            putStrLn $ "Player " ++ [player] ++ " wins!"
        else if isFull newBoard then do
            putStrLn $ displayBoard newBoard
            putStrLn "It's a draw!"
        else
            playGame newBoard (nextPlayer player)

displayBoard :: Board -> String
displayBoard board = unlines (map unwords (map (map (:[])) board)) ++ "1 2 3 4 5 6 7"

isValidMove :: Board -> Column -> Bool
isValidMove board col = any (== empty) (map (!! col) board)

dropToken :: Board -> Column -> Player -> Board
dropToken board col player = take row board ++ [newRow] ++ drop (row + 1) board
    where row = last (findEmptyRows board col)
          newRow = take col (board !! row) ++ [player] ++ drop (col + 1) (board !! row)

findEmptyRows :: Board -> Column -> [Int]
findEmptyRows board col = [i | (i, row) <- zip [0..] board, row !! col == empty]

checkWin :: Board -> Player -> Bool
checkWin board player = any id [checkRows, checkColumns, checkDiagonals]
    where
        checkRows = any (checkLine player) board
        checkColumns = any (checkLine player) (transpose board)
        checkDiagonals = any (checkLine player) (diagonals board)

checkLine :: Player -> [Player] -> Bool
checkLine player line = any (all (== player)) (windows 4 line)

windows :: Int -> [a] -> [[a]]
windows n xs
    | length xs < n = []
    | otherwise = take n xs : windows n (tail xs)

diagonals :: Board -> [[Player]]
diagonals board = diagonals' board ++ diagonals' (map reverse board)
    where
        diagonals' b = [[b !! (r + i) !! (c + i) | i <- [0..min (rows - r - 1) (columns - c - 1)]] | r <- [0..rows-1], c <- [0..columns-1], r + 3 < rows, c + 3 < columns]

isFull :: Board -> Bool
isFull board = all (all (/= empty)) board

nextPlayer :: Player -> Player
nextPlayer player
    | player == player1 = player2
    | otherwise = player1
