class Main {
    static final int ROWS = 6
    static final int COLS = 7
    char[][] board = new char[ROWS][COLS]
    char currentPlayer = 'R'

    Main() {
        // Initialize the board with empty spaces
        for (int i = 0; i < ROWS; i++) {
            for (int j = 0; j < COLS; j++) {
                board[i][j] = '.'
            }
        }
    }

    void printBoard() {
        board.each { row ->
            row.each { cell ->
                print "${cell} "
            }
            println()
        }
    }

    boolean dropDisc(int col) {
        for (int i = ROWS - 1; i >= 0; i--) {
            if (board[i][col] == '.') {
                board[i][col] = currentPlayer
                return true
            }
        }
        return false
    }

    boolean isWinningMove(int row, int col) {
        return checkDirection(row, col, 1, 0) || // Horizontal
               checkDirection(row, col, 0, 1) || // Vertical
               checkDirection(row, col, 1, 1) || // Diagonal \
               checkDirection(row, col, 1, -1)   // Diagonal /
    }

    boolean checkDirection(int row, int col, int rowDir, int colDir) {
        int count = 0
        for (int i = -3; i <= 3; i++) {
            int r = row + i * rowDir
            int c = col + i * colDir
            if (r >= 0 && r < ROWS && c >= 0 && c < COLS && board[r][c] == currentPlayer) {
                count++
                if (count == 4) return true
            } else {
                count = 0
            }
        }
        return false
    }

    void switchPlayer() {
        currentPlayer = (currentPlayer == 'R') ? 'Y' : 'R'
    }

    void play() {
        int turns = 0
        while (true) {
            printBoard()
            println "Player ${currentPlayer}, choose a column (0-${COLS - 1}):"
            int col = System.in.newReader().readLine() as int

            if (col < 0 || col >= COLS || !dropDisc(col)) {
                println "Invalid move. Try again."
                continue
            }

            int row = board.findIndexOf { it[col] == currentPlayer }

            if (isWinningMove(row, col)) {
                printBoard()
                println "Player ${currentPlayer} wins!"
                break
            }

            if (++turns == ROWS * COLS) {
                printBoard()
                println "It's a draw!"
                break
            }

            switchPlayer()
        }
    }
}

new Main().play()
