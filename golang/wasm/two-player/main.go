// main.go
package main

import (
	"fmt"
	"syscall/js"
)

const (
	rows    = 6
	columns = 7
)

var (
	board     [rows][columns]int
	turn      = 1
	gameOver  = false
	moveCount = 0
)

func dropPiece(col int) string {
	if gameOver {
		return "Game is over. Refresh to restart."
	}
	if col < 0 || col >= columns {
		return "Invalid column."
	}
	for i := rows - 1; i >= 0; i-- {
		if board[i][col] == 0 {
			board[i][col] = turn
			moveCount++
			if checkWin(i, col) {
				gameOver = true
				return fmt.Sprintf("Player %d wins!\n\n%s", turn, renderBoard())
			} else if moveCount == rows*columns {
				gameOver = true
				return fmt.Sprintf("It's a tie!\n\n%s", renderBoard())
			}
			turn = 3 - turn
			return renderBoard()
		}
	}
	return "Column full."
}

func renderBoard() string {
	output := ""
	for _, row := range board {
		for _, cell := range row {
			switch cell {
			case 0:
				output += ". "
			case 1:
				output += "X "
			case 2:
				output += "O "
			}
		}
		output += "\n"
	}
	return output
}

func checkWin(r, c int) bool {
	directions := [][2]int{
		{0, 1}, {1, 0}, {1, 1}, {1, -1},
	}
	player := board[r][c]
	for _, d := range directions {
		count := 1
		for i := 1; i < 4; i++ {
			nr, nc := r+i*d[0], c+i*d[1]
			if nr >= 0 && nr < rows && nc >= 0 && nc < columns && board[nr][nc] == player {
				count++
			} else {
				break
			}
		}
		for i := 1; i < 4; i++ {
			nr, nc := r-i*d[0], c-i*d[1]
			if nr >= 0 && nr < rows && nc >= 0 && nc < columns && board[nr][nc] == player {
				count++
			} else {
				break
			}
		}
		if count >= 4 {
			return true
		}
	}
	return false
}

func jsDrop(this js.Value, args []js.Value) interface{} {
	col := args[0].Int()
	return js.ValueOf(dropPiece(col))
}

func jsRender(this js.Value, args []js.Value) interface{} {
	return js.ValueOf(renderBoard())
}

func main() {
	js.Global().Set("dropPiece", js.FuncOf(jsDrop))
	js.Global().Set("renderBoard", js.FuncOf(jsRender))
	select {}
}
