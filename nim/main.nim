import strformat

const
  rows = 6
  cols = 7
  player1 = 'X'
  player2 = 'O'
  empty = ' '

type
  Board = array[0..rows-1, 0..cols-1, char]

proc createBoard(): Board =
  var board: Board
  for i in 0..rows-1:
    for j in 0..cols-1:
      board[i, j] = empty
  return board

proc displayBoard(board: Board) =
  for i in 0..rows-1:
    for j in 0..cols-1:
      stdout.write &"{board[i, j]} "
    stdout.write "\n"
  for j in 0..cols-1:
    stdout.write &"{j+1} "
  stdout.write "\n"

proc getColumn(): int =
  while true:
    stdout.write "Enter column (1-7): "
    let input = readLine(stdin)
    if input.len == 1 and input[0].isDigit:
      let col = parseInt(input) - 1
      if col in 0..cols-1:
        return col
    stdout.write "Invalid input. Try again.\n"

proc isValidMove(board: Board, col: int): bool =
  return board[0, col] == empty

proc dropToken(board: var Board, col: int, token: char) =
  for i in countdown(rows-1, 0):
    if board[i, col] == empty:
      board[i, col] = token
      return

proc checkLine(board: Board, token: char, x, y, dx, dy: int): bool =
  for i in 0..3:
    let nx = x + i * dx
    let ny = y + i * dy
    if not (nx in 0..rows-1 and ny in 0..cols-1) or board[nx, ny] != token:
      return false
  return true

proc checkWin(board: Board, token: char): bool =
  for i in 0..rows-1:
    for j in 0..cols-1:
      if checkLine(board, token, i, j, 1, 0) or
         checkLine(board, token, i, j, 0, 1) or
         checkLine(board, token, i, j, 1, 1) or
         checkLine(board, token, i, j, 1, -1):
        return true
  return false

proc isFull(board: Board): bool =
  for i in 0..rows-1:
    for j in 0..cols-1:
      if board[i, j] == empty:
        return false
  return true

proc playGame() =
  var board = createBoard()
  var currentPlayer = player1
  while true:
    displayBoard(board)
    echo &"Player {currentPlayer}'s turn."
    var col = getColumn()
    while not isValidMove(board, col):
      echo "Invalid move. Try again."
      col = getColumn()
    dropToken(board, col, currentPlayer)
    if checkWin(board, currentPlayer):
      displayBoard(board)
      echo &"Player {currentPlayer} wins!"
      break
    if isFull(board):
      displayBoard(board)
      echo "It's a draw!"
      break
    currentPlayer = if currentPlayer == player1: player2 else: player1

playGame()
