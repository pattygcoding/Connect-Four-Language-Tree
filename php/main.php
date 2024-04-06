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
                return true;
            }
        }
        return false; // Column is full
    }

    public function checkWin($row, $col) {
        $player = $this->board[$row][$col];

        // Check vertical
        $count = 0;
        for ($r = $row; $r < self::ROWS; $r++) {
            if ($this->board[$r][$col] === $player) {
                $count++;
            } else {
                break;
            }
        }
        if ($count >= 4) {
            return true;
        }

        // Check horizontal
        $count = 0;
        for ($c = 0; $c < self::COLS; $c++) {
            if ($this->board[$row][$c] === $player) {
                $count++;
            } else {
                $count = 0;
            }
            if ($count >= 4) {
                return true;
            }
        }

        // Check diagonal (bottom-left to top-right)
        $count = 0;
        for ($r = $row, $c = $col; $r < self::ROWS && $c < self::COLS; $r++, $c++) {
            if ($this->board[$r][$c] === $player) {
                $count++;
            } else {
                $count = 0;
            }
            if ($count >= 4) {
                return true;
            }
        }

        // Check diagonal (top-left to bottom-right)
        $count = 0;
        for ($r = $row, $c = $col; $r >= 0 && $c < self::COLS; $r--, $c++) {
            if ($this->board[$r][$c] === $player) {
                $count++;
            } else {
                $count = 0;
            }
            if ($count >= 4) {
                return true;
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
            } while ($col < 0 || $col >= self::COLS || !$this->dropPiece($col));

            if ($this->checkWin(self::ROWS - 1, $col)) {
                $this->displayBoard();
                echo "Player $currentPlayer wins!\n";
                break;
            }

            $this->player1Turn = !$this->player1Turn;
        }
    }
}

$game = new ConnectFour();
$game->play();

?>
