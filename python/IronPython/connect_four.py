class ConnectFour:
    ROWS = 6
    COLS = 7
    EMPTY = ' '

    def __init__(self):
        self.board = [[self.EMPTY] * self.COLS for _ in range(self.ROWS)]
        self.current_player = 'X'

    def print_board(self):
        for row in self.board:
            print('|'.join(row))
            print('-' * (2 * self.COLS - 1))

    def drop_piece(self, col):
        for row in reversed(self.board):
            if row[col] == self.EMPTY:
                row[col] = self.current_player
                return True
        return False

    def switch_player(self):
        self.current_player = 'O' if self.current_player == 'X' else 'X'

    def check_winner(self):
        for row in range(self.ROWS):
            for col in range(self.COLS):
                if self.board[row][col] == self.EMPTY:
                    continue
                if self.check_direction(row, col, 1, 0) or \
                   self.check_direction(row, col, 0, 1) or \
                   self.check_direction(row, col, 1, 1) or \
                   self.check_direction(row, col, 1, -1):
                    return True
        return False

    def check_direction(self, row, col, d_row, d_col):
        consecutive_count = 0
        for i in range(4):
            r = row + i * d_row
            c = col + i * d_col
            if r < 0 or r >= self.ROWS or c < 0 or c >= self.COLS:
                return False
            if self.board[r][c] == self.current_player:
                consecutive_count += 1
            else:
                break
        return consecutive_count == 4
