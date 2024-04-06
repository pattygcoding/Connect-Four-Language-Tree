package main

import (
	"fmt"
	"os"
	"strconv"
)

const (
	rows   = 6
	cols   = 7
	empty  = ' '
	player1 = 'X'
	player2 = 'O'
)

type ConnectFour struct {
	board         [][]rune
	player1Turn   bool
	lastMoveRow   int
	lastMoveCol   int
}

func NewConnectFour() *ConnectFour {
	board := make([][]rune, rows)
	for i := range board {
		board[i] = make([]rune, cols)
		for j := range board[i] {
			board[i][j] = empty
		}
	}
	return &ConnectFour{
		board:       board,
		player1Turn: true,
	}
}

func (game *ConnectFour) DisplayBoard() {
	for _, row := range game.board {
		fmt.Println(string(row))
	}
	fmt.Println("1 2 3 4 5 6 7")
	fmt.Println()
}

func (game *ConnectFour) DropPiece(col int) bool {
	for row := rows - 1; row >= 0; row-- {
		if game.board[row][col] == empty {
			if game.player1Turn {
				game.board[row][col] = player1
			} else {
				game.board[row][col] = player2
			}
			game.lastMoveRow = row
			game.lastMoveCol = col
			return true
		}
	}
	return false // Column is full
}

func (game *ConnectFour) CheckWin() bool {
	player := game.board[game.lastMoveRow][game.lastMoveCol]

	// Check vertical
	count := 0
	for r := game.lastMoveRow; r < rows; r++ {
		if game.board[r][game.lastMoveCol] == player {
			count++
		} else {
			break
		}
	}
	if count >= 4 {
		return true
	}

	// Check horizontal
	count = 0
	for c := 0; c < cols; c++ {
		if game.board[game.lastMoveRow][c] == player {
			count++
		} else {
			count = 0
		}
		if count >= 4 {
			return true
		}
	}

	// Check diagonal (bottom-left to top-right)
	count = 0
	for r, c := game.lastMoveRow, game.lastMoveCol; r < rows && c < cols; r, c = r+1, c+1 {
		if game.board[r][c] == player {
			count++
		} else {
			count = 0
		}
		if count >= 4 {
			return true
		}
	}

	// Check diagonal (top-left to bottom-right)
	count = 0
	for r, c := game.lastMoveRow, game.lastMoveCol; r >= 0 && c < cols; r, c = r-1, c+1 {
		if game.board[r][c] == player {
			count++
		} else {
			count = 0
		}
		if count >= 4 {
			return true
		}
	}

	return false
}

func main() {
	game := NewConnectFour()

	for {
		game.DisplayBoard()

		var currentPlayer rune
		if game.player1Turn {
			currentPlayer = player1
		} else {
			currentPlayer = player2
		}
		fmt.Printf("Player %c's turn. Enter column (1-7): ", currentPlayer)

		var col int
		fmt.Scanln(&col)
		col-- // Convert to zero-based index

		if col < 0 || col >= cols || !game.DropPiece(col) {
			fmt.Println("Invalid move. Please try again.")
			continue
		}

		if game.CheckWin() {
			game.DisplayBoard()
			fmt.Printf("Player %c wins!\n", currentPlayer)
			break
		}

		game.player1Turn = !game.player1Turn
	}
}
