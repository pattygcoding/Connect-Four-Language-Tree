fn main() {
    mut board := create_board()
    mut current_player := 'X'
    for {
        display_board(board)
        println('Player $current_player\'s turn. Enter column (1-7): ')
        col := get_column() - 1
        if col < 0 || col >= board[0].len || !is_valid_move(board, col) {
            println('Invalid move. Try again.')
            continue
        }
        board = drop_token(board, col, current_player)
        if check_win(board, current_player) {
            display_board(board)
            println('Player $current_player wins!')
            break
        } else if is_full(board) {
            display_board(board)
            println('It\'s a draw!')
            break
        }
        current_player = if current_player == 'X' { 'O' } else { 'X' }
    }
}

fn create_board() [][]rune {
    rows, cols := 6, 7
    mut board := [][]rune{len: rows, init: []rune{len: cols, init: ` `}}
    return board
}

fn display_board(board [][]rune) {
    for row in board {
        println(row.map(fn (c rune) string { return c.str() }).join(' '))
    }
    println((1..8).map(fn (i int) string { return '$i' }).join(' '))
}

fn get_column() int {
    col := os.input('> ').int()
    return col
}

fn is_valid_move(board [][]rune, col int) bool {
    return board[0][col] == ` `
}

fn drop_token(mut board [][]rune, col int, token rune) [][]rune {
    for i in board.len - 1 .. -1 {
        if board[i][col] == ` ` {
            board[i][col] = token
            break
        }
    }
    return board
}

fn check_win(board [][]rune, token rune) bool {
    for i in 0 .. board.len {
        for j in 0 .. board[0].len {
            if check_line(board, token, i, j, 1, 0) || check_line(board, token, i, j, 0, 1) || check_line(board, token, i, j, 1, 1) || check_line(board, token, i, j, 1, -1) {
                return true
            }
        }
    }
    return false
}

fn check_line(board [][]rune, token rune, x int, y int, dx int, dy int) bool {
    count := 0
    for i in 0 .. 4 {
        nx, ny := x + i * dx, y + i * dy
        if nx >= 0 && nx < board.len && ny >= 0 && ny < board[0].len && board[nx][ny] == token {
            count++
        } else {
            break
        }
    }
    return count == 4
}

fn is_full(board [][]rune) bool {
    for row in board {
        for cell in row {
            if cell == ` ` {
                return false
            }
        }
    }
    return true
}
