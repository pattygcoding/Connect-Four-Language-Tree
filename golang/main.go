package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const (
	rows = 6
	cols = 7
)

var (
	playerTokens = [2]string{"X", "O"}
)

func main() {
	board := make([][]string, rows)
	for i := range board {
		board[i] = make([]string, cols)
		for j := range board[i] {
			board[i][j] = " "
		}
	}

	currentPlayer := 0
	gameOver := false
	scanner := bufio.NewScanner(os.Stdin)

	for !gameOver {
		printBoard(board)
		fmt.Printf("Player %d's turn (%s). Enter column (1-%d): ", currentPlayer+1, playerTokens[currentPlayer], cols)
		scanner.Scan()
		input := scanner.Text()
		col, err := strconv.Atoi(strings.TrimSpace(input))
		if err != nil || col < 1 || col > cols {
			fmt.Println("Invalid column. Try again.")
			continue
		}
		col-- // Adjust for zero-indexed slice

		if !dropPiece(board, col, playerTokens[currentPlayer]) {
			fmt.Println("Column is full. Try another one.")
			continue
		}

		if checkWin(board, playerTokens[currentPlayer]) {
			printBoard(board)
			fmt.Printf("Player %d wins!\n", currentPlayer+1)
			gameOver = true
		} else if isBoardFull(board) {
			printBoard(board)
			fmt.Println("It's a draw!")
			gameOver = true
		}

		currentPlayer = 1 - currentPlayer // Switch player
	}
}

func printBoard(board [][]string) {
	fmt.Println("\n 1 2 3 4 5 6 7")
	for _, row := range board {
		fmt.Print("|")
		for _, cell := range row {
			fmt.Printf("%s|", cell)
		}
		fmt.Println()
	}
	fmt.Println("---------------")
}

func dropPiece(board [][]string, col int, playerToken string) bool {
	for i := rows - 1; i >= 0; i-- {
		if board[i][col] == " " {
			board[i][col] = playerToken
			return true
		}
	}
	return false
}

func checkWin(board [][]string, playerToken string) bool {
	// Check horizontal
	for i := 0; i < rows; i++ {
		for j := 0; j < cols-3; j++ {
			if board[i][j] == playerToken && board[i][j+1] == playerToken && board[i][j+2] == playerToken && board[i][j+3] == playerToken {
				return true
			}
		}
	}
	// Check vertical
	for i := 0; i < rows-3; i++ {
		for j := 0; j < cols; j++ {
			if board[i][j] == playerToken && board[i+1][j] == playerToken && board[i+2][j] == playerToken && board[i+3][j] == playerToken {
				return true
			}
		}
	}
	// Check diagonal (bottom left to top right)
	for i := 3; i < rows; i++ {
		for j := 0; j < cols-3; j++ {
			if board[i][j] == playerToken && board[i-1][j+1] == playerToken && board[i-2][j+2] == playerToken && board[i-3][j+3] == playerToken {
				return true
			}
		}
	}
	// Check diagonal (top left to bottom right)
	for i := 0; i < rows-3; i++ {
		for j := 0; j < cols-3; j++ {
			if board[i][j] == playerToken && board[i+1][j+1] == playerToken && board[i+2][j+2] == playerToken && board[i+3][j+3] == playerToken {
				return true
			}
		}
	}
	return false
}

func isBoardFull(board [][]string) bool {
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			if board[i][j] == " " {
				return false
			}
		}
	}
	return true
}
