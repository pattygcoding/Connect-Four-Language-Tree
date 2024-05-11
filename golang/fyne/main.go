package main

import (
    "fmt"
    "fyne.io/fyne/v2"
    "fyne.io/fyne/v2/app"
    "fyne.io/fyne/v2/container"
    "fyne.io/fyne/v2/widget"
)

const (
    rows    = 6
    columns = 7
)

var (
    currentPlayer = "R" // "R" for Red, "Y" for Yellow
    board         [rows][columns]string
    buttons       [rows][columns]*widget.Button
    statusLabel   *widget.Label
)

func main() {
    a := app.New()
    w := a.NewWindow("Connect Four")

    statusLabel = widget.NewLabel("Current Player: R")

    grid := container.NewGridWithColumns(columns)

    // Initialize buttons and the grid layout
    for i := 0; i < rows; i++ {
        for j := 0; j < columns; j++ {
            btn := widget.NewButton("", func(row, col int) func() {
                return func() {
                    makeMove(row, col)
                }
            }(i, j))
            buttons[i][j] = btn
            grid.Add(btn)
        }
    }

    restartButton := widget.NewButton("Restart", restartGame)

    w.SetContent(container.NewVBox(
        statusLabel,
        grid,
        restartButton,
    ))
    w.Resize(fyne.NewSize(640, 480))
    w.ShowAndRun()
}

func makeMove(row, col int) {
    // Find the lowest empty space in the column
    for i := rows - 1; i >= 0; i-- {
        if board[i][col] == "" {
            board[i][col] = currentPlayer
            buttons[i][col].SetText(currentPlayer)
            if checkWinner(i, col) {
                statusLabel.SetText(fmt.Sprintf("Player %s Wins!", currentPlayer))
                disableButtons()
                return
            }
            switchPlayer()
            return
        }
    }
}

func switchPlayer() {
    if currentPlayer == "R" {
        currentPlayer = "Y"
    } else {
        currentPlayer = "R"
    }
    statusLabel.SetText("Current Player: " + currentPlayer)
}

func checkWinner(row, col int) bool {
    // Horizontal, vertical, diagonal checks
    return checkLine(row, col, 0, 1) || // Horizontal
        checkLine(row, col, 1, 0) || // Vertical
        checkLine(row, col, 1, 1) || // Diagonal (down-right)
        checkLine(row, col, 1, -1)  // Diagonal (down-left)
}

func checkLine(row, col, dRow, dCol int) bool {
    count := 1
    count += countLine(row, col, dRow, dCol)
    count += countLine(row, col, -dRow, -dCol)
    return count >= 4
}

func countLine(row, col, dRow, dCol int) int {
    player := currentPlayer
    count := 0
    i, j := row+dRow, col+dCol
    for i >= 0 && i < rows && j >= 0 && j < columns && board[i][j] == player {
        count++
        i += dRow
        j += dCol
    }
    return count
}

func disableButtons() {
    for i := 0; i < rows; i++ {
        for j := 0; j < columns; j++ {
            buttons[i][j].Disable()
        }
    }
}

func restartGame() {
    for i := 0; i < rows; i++ {
        for j := 0; j < columns; j++ {
            board[i][j] = ""
            buttons[i][j].Enable()
            buttons[i][j].SetText("")
        }
    }
    currentPlayer = "R"
    statusLabel.SetText("Current Player: R")
}

