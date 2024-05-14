<?php

class ConnectFour {
    const ROWS = 6;
    const COLS = 7;
    const EMPTY = ' ';
    const PLAYER1 = 'X';
    const PLAYER2 = 'O';

    private $board;
    private $player1Turn;

    public function __construct() {
        $this->board = array_fill(0, self::ROWS, array_fill(0, self::COLS, self::EMPTY));
        $this->player1Turn = true;
    }

    public function displayBoard() {
        echo " 1 2 3 4 5 6 7\n";
        foreach ($this->board as $row) {
            echo "|" . implode("|", $row) . "|\n";
        }
        echo "---------------\n";
    }

    public function dropPiece($col) {
        for ($row = self::ROWS - 1; $row >= 0; $row--) {
            if ($this->board[$row][$col] === self::EMPTY) {
                $this->board[$row][$col] = $this->player1Turn ? self::PLAYER1 : self::PLAYER2;
                return $row;
            }
        }
        return -1; // Column is full
    }

    public function checkWin($row, $col) {
        $player = $this->board[$row][$col];
        return $this->checkDirection($row, $col, 0, 1, $player) || // Horizontal
               $this->checkDirection($row, $col, 1, 0, $player) || // Vertical
               $this->checkDirection($row, $col, 1, 1, $player) || // Diagonal \
               $this->checkDirection($row, $col, 1, -1, $player);  // Diagonal /
    }

    private function checkDirection($row, $col, $rowDir, $colDir, $player) {
        $count = 0;
        for ($i = -3; $i <= 3; $i++) {
            $r = $row + $i * $rowDir;
            $c = $col + $i * $colDir;
            if ($r >= 0 && $r < self::ROWS && $c >= 0 && $c < self::COLS && $this->board[$r][$c] === $player) {
                $count++;
                if ($count === 4) {
                    return true;
                }
            } else {
                $count = 0;
            }
        }
        return false;
    }

    public function play() {
        while (true) {
            $this->displayBoard();

            $currentPlayer = $this->player1Turn ? self::PLAYER1 : self::PLAYER2;
            echo "Player $currentPlayer's turn.\n";

            do {
                echo "Enter column (1-7): ";
                $col = intval(fgets(STDIN)) - 1;
            } while ($col < 0 || $col >= self::COLS || ($row = $this->dropPiece($col)) === -1);

            if ($this->checkWin($row, $col)) {
                $this->displayBoard();
                echo "Player $currentPlayer wins!\n";
                break;
            }

            if ($this->isBoardFull()) {
                $this->displayBoard();
                echo "The game is a draw!\n";
                break;
            }

            $this->player1Turn = !$this->player1Turn;
        }
    }

    private function isBoardFull() {
        for ($col = 0; $col < self::COLS; $col++) {
            if ($this->board[0][$col] === self::EMPTY) {
                return false;
            }
        }
        return true;
    }
}

$game = new ConnectFour();
$game->play();

?>
