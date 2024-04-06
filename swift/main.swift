import Foundation

let rows = 6
let cols = 7
let empty: Character = " "
let player1: Character = "X"
let player2: Character = "O"

class ConnectFour {
    var board: [[Character]]
    var player1Turn: Bool
    var lastMoveRow: Int
    var lastMoveCol: Int

    init() {
        board = Array(repeating: Array(repeating: empty, count: cols), count: rows)
        player1Turn = true
        lastMoveRow = 0
        lastMoveCol = 0
    }

    func displayBoard() {
        print(" 1 2 3 4 5 6 7")
        for row in board {
            print("|" + row.map(String.init).joined(separator: "|") + "|")
        }
        print("---------------")
    }

    func dropPiece(col: Int) -> Bool {
        for row in (0..<rows).reversed() {
            if board[row][col] == empty {
                board[row][col] = player1Turn ? player1 : player2
                lastMoveRow = row
                lastMoveCol = col
                return true
            }
        }
        return false // Column is full
    }

    func checkWin() -> Bool {
        let player = board[lastMoveRow][lastMoveCol]

        // Check vertical
        var count = 0
        for r in lastMoveRow..<rows {
            if board[r][lastMoveCol] == player {
                count += 1
            } else {
                break
            }
        }
        if count >= 4 {
            return true
        }

        // Check horizontal
        count = 0
        for c in 0..<cols {
            if board[lastMoveRow][c] == player {
                count += 1
            } else {
                count = 0
            }
            if count >= 4 {
                return true
            }
        }

        // Check diagonal (bottom-left to top-right)
        count = 0
        for (r, c) in zip(lastMoveRow..<rows, lastMoveCol..<cols) {
            if board[r][c] == player {
                count += 1
            } else {
                count = 0
            }
            if count >= 4 {
                return true
            }
        }

        // Check diagonal (top-left to bottom-right)
        count = 0
        for (r, c) in zip((0...lastMoveRow).reversed(), lastMoveCol..<cols) {
            if board[r][c] == player {
                count += 1
            } else {
                count = 0
            }
            if count >= 4 {
                return true
            }
        }

        return false
    }

    func play() {
        while true {
            displayBoard()

            let currentPlayer = player1Turn ? player1 : player2
            print("Player \(currentPlayer)'s turn.")
            print("Enter column (1-7): ")

            guard let input = readLine(), let col = Int(input) else {
                print("Invalid input. Please enter a valid column number.")
                continue
            }

            let columnIndex = col - 1 // Convert to zero-based index
            if columnIndex < 0 || columnIndex >= cols || !dropPiece(col: columnIndex) {
                print("Invalid move. Please try again.")
                continue
            }

            if checkWin() {
                displayBoard()
                print("Player \(currentPlayer) wins!")
                break
            }

            player1Turn = !player1Turn
        }
    }
}

let game = ConnectFour()
game.play()
