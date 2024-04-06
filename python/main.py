class Connect4:
    def __init__(self):
        self.rows = 6
        self.cols = 7
        self.board = [[' ' for _ in range(self.cols)] for _ in range(self.rows)]
        self.current_player = 1

    def display_board(self):
        print()
        for row in self.board:
            print('| ' + ' | '.join(row) + ' |')
        print('-' * (4 * self.cols + 1))
        print('| 1 | 2 | 3 | 4 | 5 | 6 | 7 |')

    def drop_piece(self, col):
        if col < 0 or col >= self.cols or self.board[0][col] != ' ':
            print("Invalid move. Please try again.")
            return False

        for row in range(self.rows - 1, -1, -1):
            if self.board[row][col] == ' ':
                self.board[row][col] = 'X' if self.current_player == 1 else 'O'
                return True

    def check_win(self):
        player_piece = 'X' if self.current_player == 1 else 'O'

        # Check horizontal
        for row in range(self.rows):
            for col in range(self.cols - 3):
                if all(self.board[row][col + i] == player_piece for i in range(4)):
                    return True

        # Check vertical
        for col in range(self.cols):
            for row in range(self.rows - 3):
                if all(self.board[row + i][col] == player_piece for i in range(4)):
                    return True

        # Check diagonals
        for row in range(self.rows - 3):
            for col in range(self.cols - 3):
                if all(self.board[row + i][col + i] == player_piece for i in range(4)):
                    return True
            for col in range(3, self.cols):
                if all(self.board[row + i][col - i] == player_piece for i in range(4)):
                    return True

        return False

    def play(self):
        self.display_board()

        while True:
            try:
                col = int(input(f"Player {self.current_player}, enter column (1-7): ")) - 1
            except ValueError:
                print("Invalid input. Please enter a number (1-7).")
                continue

            if self.drop_piece(col):
                if self.check_win():
                    self.display_board()
                    print(f"Player {self.current_player} wins!")
                    break

                if all(self.board[0][col] != ' ' for col in range(self.cols)):
                    self.display_board()
                    print("It's a draw!")
                    break

                self.current_player = 2 if self.current_player == 1 else 1
                self.display_board()


if __name__ == '__main__':
    game = Connect4()
    game.play()
