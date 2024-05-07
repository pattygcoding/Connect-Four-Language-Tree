fun main() {
    val game = ConnectFour()
    game.playGame()
}

class ConnectFour {
    private val rows = 6
    private val columns = 7
    private val board = Array(rows) { Array(columns) { ' ' } }
    private val playerOneToken = 'A'
    private val playerTwoToken = 'B'
    private var currentPlayer = playerOneToken

    fun playGame() {
        var turns = 0
        while (true) {
            printBoard()
            println("Player $currentPlayer's turn. Choose column (1-7):")

            var column: Int
            while (true) {
                val input = readLine() ?: ""
                column = input.toIntOrNull()?.minus(1) ?: -1
                if (column in 0 until columns && placeToken(currentPlayer, column)) {
                    break
                }
                println("Invalid column. Try again.")
            }

            turns++
            if (checkWinner(currentPlayer)) {
                printBoard()
                println("Player $currentPlayer wins!")
                break
            }

            if (turns == rows * columns) {
                printBoard()
                println("It's a draw!")
                break
            }

            currentPlayer = if (currentPlayer == playerOneToken) playerTwoToken else playerOneToken
        }
    }

    private fun placeToken(player: Char, column: Int): Boolean {
        for (row in rows - 1 downTo 0) {
            if (board[row][column] == ' ') {
                board[row][column] = player
                return true
            }
        }
        return false
    }

    private fun checkWinner(player: Char): Boolean {
        // Horizontal, vertical, and diagonal checks
        for (row in 0 until rows) {
            for (col in 0 until columns - 3) {
                if (isWinningCombo(row, col, 0, 1, player)) return true
            }
        }

        for (col in 0 until columns) {
            for (row in 0 until rows - 3) {
                if (isWinningCombo(row, col, 1, 0, player)) return true
            }
        }

        for (row in 0 until rows - 3) {
            for (col in 0 until columns - 3) {
                if (isWinningCombo(row, col, 1, 1, player)) return true
            }
        }

        for (row in 3 until rows) {
            for (col in 0 until columns - 3) {
                if (isWinningCombo(row, col, -1, 1, player)) return true
            }
        }

        return false
    }

    private fun isWinningCombo(row: Int, col: Int, rowStep: Int, colStep: Int, player: Char): Boolean {
        return (0..3).all { i ->
            board[row + i * rowStep][col + i * colStep] == player
        }
    }

    private fun printBoard() {
        println()
        for (row in board) {
            println("|" + row.joinToString("|") + "|")
        }
        println("+-+-+-+-+-+-+-+")
        println(" 1 2 3 4 5 6 7 ")
        println()
    }
}
