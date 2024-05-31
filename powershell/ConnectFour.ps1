function Initialize-Board {
    $board = @()
    for ($i = 0; $i -lt 6; $i++) {
        $board += @(0, 0, 0, 0, 0, 0, 0)
    }
    return $board
}

function Display-Board {
    param (
        [array]$board
    )

    Write-Host " 1 2 3 4 5 6 7"
    for ($i = 0; $i -lt 6; $i++) {
        for ($j = 0; $j -lt 7; $j++) {
            $cell = $board[$i][$j]
            if ($cell -eq 0) {
                Write-Host -NoNewline " ."
            } elseif ($cell -eq 1) {
                Write-Host -NoNewline " X"
            } else {
                Write-Host -NoNewline " O"
            }
        }
        Write-Host ""
    }
    Write-Host ""
}

# Check for a win condition
function Check-Win {
    param (
        [array]$board,
        [int]$player
    )

    # Check horizontal
    for ($i = 0; $i -lt 6; $i++) {
        for ($j = 0; $j -lt 4; $j++) {
            if ($board[$i][$j] -eq $player -and $board[$i][$j+1] -eq $player -and $board[$i][$j+2] -eq $player -and $board[$i][$j+3] -eq $player) {
                return $true
            }
        }
    }

    # Check vertical
    for ($i = 0; $i -lt 3; $i++) {
        for ($j = 0; $j -lt 7; $j++) {
            if ($board[$i][$j] -eq $player -and $board[$i+1][$j] -eq $player -and $board[$i+2][$j] -eq $player -and $board[$i+3][$j] -eq $player) {
                return $true
            }
        }
    }

    # Check diagonal (bottom-left to top-right)
    for ($i = 3; $i -lt 6; $i++) {
        for ($j = 0; $j -lt 4; $j++) {
            if ($board[$i][$j] -eq $player -and $board[$i-1][$j+1] -eq $player -and $board[$i-2][$j+2] -eq $player -and $board[$i-3][$j+3] -eq $player) {
                return $true
            }
        }
    }

    # Check diagonal (top-left to bottom-right)
    for ($i = 0; $i -lt 3; $i++) {
        for ($j = 0; $j -lt 4; $j++) {
            if ($board[$i][$j] -eq $player -and $board[$i+1][$j+1] -eq $player -and $board[$i+2][$j+2] -eq $player -and $board[$i+3][$j+3] -eq $player) {
                return $true
            }
        }
    }

    return $false
}

function Drop-Piece {
    param (
        [array]$board,
        [int]$column,
        [int]$player
    )

    for ($i = 5; $i -ge 0; $i--) {
        if ($board[$i][$column] -eq 0) {
            $board[$i][$column] = $player
            return $true
        }
    }
    return $false
}

function Play-ConnectFour {
    $board = Initialize-Board
    $currentPlayer = 1
    $moves = 0

    while ($true) {
        Display-Board -board $board
        Write-Host "Player $currentPlayer's turn. Choose a column (1-7):"
        $column = Read-Host

        if ($column -match '^[1-7]$') {
            $column = [int]$column - 1
            if (Drop-Piece -board $board -column $column -player $currentPlayer) {
                $moves++
                if (Check-Win -board $board -player $currentPlayer) {
                    Display-Board -board $board
                    Write-Host "Player $currentPlayer wins!"
                    break
                } elseif ($moves -eq 42) {
                    Display-Board -board $board
                    Write-Host "It's a draw!"
                    break
                }
                $currentPlayer = 3 - $currentPlayer
            } else {
                Write-Host "Column is full. Try a different column."
            }
        } else {
            Write-Host "Invalid input. Please enter a number between 1 and 7."
        }
    }
}

Play-ConnectFour