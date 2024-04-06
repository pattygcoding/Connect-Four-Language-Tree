import scala.util.{Try, Success, Failure}
import scala.io.StdIn

object ConnectFour {

  val ROWS = 6
  val COLS = 7
  val EMPTY = ' '
  val PLAYER1 = 'X'
  val PLAYER2 = 'O'

  def main(args: Array[String]): Unit = {
    // Initialize the game board
    var board = Array.fill(ROWS, COLS)(EMPTY)

    var currentPlayer = PLAYER1
    var gameOver = false

    while (!gameOver) {
      displayBoard(board)

      // Prompt current player for column input
      println(s"Player $currentPlayer's turn. Enter column (1-$COLS): ")
      val col = getUserInput() match {
        case Success(c) => c - 1
        case Failure(_) => {
          println("Invalid input. Please enter a valid column number.")
          -1
        }
      }

      if (col >= 0 && col < COLS && board(0)(col) == EMPTY) {
        // Drop the piece into the board
        dropPiece(board, col, currentPlayer)

        // Check for win condition
        if (checkWin(board, currentPlayer)) {
          displayBoard(board)
          println(s"Player $currentPlayer wins!")
          gameOver = true
        } else if (board.flatten.forall(_ != EMPTY)) {
          displayBoard(board)
          println("It's a draw! The board is full.")
          gameOver = true
        } else {
          // Switch to the next player
          currentPlayer = if (currentPlayer == PLAYER1) PLAYER2 else PLAYER1
        }
      } else {
        println("Invalid move. Please choose another column.")
      }
    }
  }

  def displayBoard(board: Array[Array[Char]]): Unit = {
    println(" 1 2 3 4 5 6 7")
    board.foreach(row => println("|" + row.mkString("|") + "|"))
    println("---------------")
  }

  def dropPiece(board: Array[Array[Char]], col: Int, player: Char): Unit = {
    val row = board.indexWhere(_(col) == EMPTY)
    board(row)(col) = player
  }

  def checkWin(board: Array[Array[Char]], player: Char): Boolean = {
    def isWinningSequence(seq: List[Char]): Boolean = {
      seq.mkString.contains(s"$player$player$player$player")
    }

    // Check horizontal
    for (row <- 0 until ROWS; col <- 0 to COLS - 4) {
      if (isWinningSequence(board(row).slice(col, col + 4).toList))
        return true
    }

    // Check vertical
    for (row <- 0 to ROWS - 4; col <- 0 until COLS) {
      if (isWinningSequence(List(board(row)(col), board(row + 1)(col), board(row + 2)(col), board(row + 3)(col))))
        return true
    }

    // Check diagonal (top-left to bottom-right)
    for (row <- 0 to ROWS - 4; col <- 0 to COLS - 4) {
      if (isWinningSequence(List(board(row)(col), board(row + 1)(col + 1), board(row + 2)(col + 2), board(row + 3)(col + 3))))
        return true
    }

    // Check diagonal (top-right to bottom-left)
    for (row <- 0 to ROWS - 4; col <- 3 until COLS) {
      if (isWinningSequence(List(board(row)(col), board(row + 1)(col - 1), board(row + 2)(col - 2), board(row + 3)(col - 3))))
        return true
    }

    false
  }

  def getUserInput(): Try[Int] = {
    Try(StdIn.readLine().trim.toInt)
  }
}
