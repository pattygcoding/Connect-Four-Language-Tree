use std::io;

const ROWS: usize = 6;
const COLS: usize = 7;
const EMPTY: char = ' ';
const PLAYER1: char = 'X';
const PLAYER2: char = 'O';

type Board = [[char; COLS]; ROWS];

fn initialize_board() -> Board {
    [[EMPTY; COLS]; ROWS]
}

fn print_board(board: &Board) {
    for row in board.iter() {
        for cell in row.iter() {
            print!("| {}", cell);
        }
        println!("|");
    }
    for _ in 0..COLS {
        print!("---");
    }
    println!();
    for col in 0..COLS {
        print!("  {}", col);
    }
    println!();
}

fn is_valid_move(board: &Board, col: usize) -> bool {
    col < COLS && board[0][col] == EMPTY
}

fn make_move(board: &mut Board, col: usize, player: char) -> bool {
    if !is_valid_move(board, col) {
        return false;
    }

    for row in (0..ROWS).rev() {
        if board[row][col] == EMPTY {
            board[row][col] = player;
            return true;
        }
    }
    false
}

fn check_direction(board: &Board, row: usize, col: usize, row_dir: isize, col_dir: isize, player: char) -> bool {
    let mut count = 0;
    for i in 0..4 {
        let r = row as isize + i * row_dir;
        let c = col as isize + i * col_dir;
        if r >= 0 && r < ROWS as isize && c >= 0 && c < COLS as isize && board[r as usize][c as usize] == player {
            count += 1;
        } else {
            break;
        }
    }
    count == 4
}

fn check_win(board: &Board, player: char) -> bool {
    for row in 0..ROWS {
        for col in 0..COLS {
            if board[row][col] == player {
                if check_direction(board, row, col, 0, 1, player) ||  // Horizontal
                   check_direction(board, row, col, 1, 0, player) ||  // Vertical
                   check_direction(board, row, col, 1, 1, player) ||  // Diagonal \
                   check_direction(board, row, col, 1, -1, player) { 
                    return true;
                }
            }
        }
    }
    false
}

fn is_board_full(board: &Board) -> bool {
    for col in 0..COLS {
        if board[0][col] == EMPTY {
            return false;
        }
    }
    true
}

fn main() {
    let mut board = initialize_board();
    let mut current_player = PLAYER1;

    loop {
        print_board(&board);
        println!("Player {}, enter column (0-{}): ", current_player, COLS - 1);
        
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read line");
        let col: usize = match input.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Invalid input. Please enter a number between 0 and {}.", COLS - 1);
                continue;
            }
        };

        if !make_move(&mut board, col, current_player) {
            println!("Invalid move. Try again.");
            continue;
        }

        if check_win(&board, current_player) {
            print_board(&board);
            println!("Player {} wins!", current_player);
            break;
        }

        if is_board_full(&board) {
            print_board(&board);
            println!("The game is a draw!");
            break;
        }

        current_player = if current_player == PLAYER1 { PLAYER2 } else { PLAYER1 };
    }
}
