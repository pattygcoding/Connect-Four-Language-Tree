import json
import sys

# Constants
ROWS = 6
COLS = 7
EMPTY = ' '
PLAYER1 = 'X'
PLAYER2 = 'O'

# File paths
JSON_FILE = 'board.json'

def load_game_state():
    try:
        with open(JSON_FILE, 'r') as file:
            return json.load(file)
    except FileNotFoundError:
        # Initialize new game state if file doesn't exist
        return {
            'board': [[EMPTY] * COLS for _ in range(ROWS)],
            'current_player': PLAYER1
        }

def save_game_state(game_state):
    with open(JSON_FILE, 'w') as file:
        json.dump(game_state, file)

def display_board(board):
    for row in board:
        print('| ' + ' | '.join(row) + ' |')
        print('+' + '-' * (4 * COLS - 1) + '+')

def drop_piece(board, col, player):
    for row in range(ROWS - 1, -1, -1):
        if board[row][col] == EMPTY:
            board[row][col] = player
            return True
    return False

def check_win(board, player):
    # Check horizontal
    for row in range(ROWS):
        for col in range(COLS - 3):
            if all(board[row][col + i] == player for i in range(4)):
                return True

    # Check vertical
    for col in range(COLS):
        for row in range(ROWS - 3):
            if all(board[row + i][col] == player for i in range(4)):
                return True

    # Check diagonal (top-left to bottom-right)
    for row in range(ROWS - 3):
        for col in range(COLS - 3):
            if all(board[row + i][col + i] == player for i in range(4)):
                return True

    # Check diagonal (bottom-left to top-right)
    for row in range(3, ROWS):
        for col in range(COLS - 3):
            if all(board[row - i][col + i] == player for i in range(4)):
                return True

    return False

def main():
    game_state = load_game_state()
    board = game_state['board']
    current_player = game_state['current_player']

    while True:
        display_board(board)
        col = int(input(f"Player {current_player}'s turn. Enter column (1-7): ")) - 1

        if 0 <= col < COLS and board[0][col] == EMPTY:
            if drop_piece(board, col, current_player):
                if check_win(board, current_player):
                    display_board(board)
                    print(f"Player {current_player} wins!")
                    break
                elif all(board[0][col] != EMPTY for col in range(COLS)):
                    display_board(board)
                    print("It's a draw! The board is full.")
                    break
                else:
                    current_player = PLAYER2 if current_player == PLAYER1 else PLAYER1
                    game_state['current_player'] = current_player
                    save_game_state(game_state)
        else:
            print("Invalid move. Please choose a valid column (1-7).")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nGame interrupted. Exiting.")
        sys.exit(0)
