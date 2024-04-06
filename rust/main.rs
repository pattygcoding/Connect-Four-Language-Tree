use std::io;

const ROWS: usize = 6;
const COLS: usize = 7;
const EMPTY: char = ' ';
const PLAYER1: char = 'X';
const PLAYER2: char = 'O';

struct ConnectFour {
    board: [[char; COLS]; ROWS],
    player1_turn: bool,
}

impl ConnectFour {
    fn new() -> Self {
        ConnectFour {
            board: [[EMPTY; COLS]; ROWS],
            player1_turn: true,
        }
    }

    fn display_board(&self) {
        println!(" 1 2 3 4 5 6 7");
        for row in &self.board {
            println!("|{}|", row.iter().collect::<String>());
        }
        println!("---------------");
    }

    fn drop_piece(&mut self, col: usize) -> bool {
        for row in (0..ROWS).rev() {
            if self.board[row][col] == EMPTY {
                self.board[row][col] = if self.player1_turn { PLAYER1 } else { PLAYER2 };
                return true;
            }
        }
        false // Column is full
    }

    fn check_win(&self, row: usize, col: usize) -> bool {
        let player = self.board[row][col];

        // Check vertical
        let mut count = 0;
        for r in row..ROWS {
            if self.board[r][col] == player {
                count += 1;
            } else {
                break;
            }
        }
        if count >= 4 {
            return true;
        }

        // Check horizontal
        count = 0;
        for c in 0..COLS {
            if self.board[row][c] == player {
                count += 1;
            } else {
                count = 0;
            }
            if count >= 4 {
                return true;
            }
        }

        // Check diagonal (bottom-left to top-right)
        count = 0;
        for (r, c) in (row..ROWS).zip(col..COLS) {
            if self.board[r][c] == player {
                count += 1;
            } else {
                count = 0;
            }
            if count >= 4 {
                return true;
            }
        }

        // Check diagonal (top-left to bottom-right)
        count = 0;
        for (r, c) in (0..=row).rev().zip(col..COLS) {
            if self.board[r][c] == player {
                count += 1;
            } else {
                count = 0;
            }
            if count >= 4 {
                return true;
            }
        }

        false
    }

    fn play(&mut self) {
        loop {
            self.display_board();

            let current_player = if self.player1_turn { PLAYER1 } else { PLAYER2 };
            println!("Player {}'s turn.", current_player);

            let col: usize;
            loop {
                println!("Enter column (1-7): ");
                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("Failed to read input.");
                col = match input.trim().parse::<usize>() {
                    Ok(num) => num - 1, // Convert to zero-based index
                    Err(_) => continue,
                };

                if col >= COLS || !self.drop_piece(col) {
                    println!("Invalid move. Please try again.");
                    continue;
                }

                break;
            }

            let row = self.board.iter().position(|&row| row[col] != EMPTY).unwrap();
            if self.check_win(row, col) {
                self.display_board();
                println!("Player {} wins!", current_player);
                break;
            }

            self.player1_turn = !self.player1_turn;
        }
    }
}

fn main() {
    let mut game = ConnectFour::new();
    game.play();
}
