#include <stdio.h>
#include <stdbool.h>

#define ROWS 6
#define COLS 7

void initialize_board(char board[ROWS][COLS]) {
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            board[i][j] = ' ';
        }
    }
}

void display_board(char board[ROWS][COLS]) {
    printf("\n");
    printf("| 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n");
    for (int i = 0; i < ROWS; i++) {
        printf("| ");
        for (int j = 0; j < COLS; j++) {
            printf("%c | ", board[i][j]);
        }
        printf("\n");
    }

}

int is_column_full(char board[ROWS][COLS], int col) {
    return board[0][col] != ' ';
}

void drop_piece(char board[ROWS][COLS], int col, char piece) {
    for (int i = ROWS - 1; i >= 0; i--) {
        if (board[i][col] == ' ') {
            board[i][col] = piece;
            break;
        }
    }
}

int check_win(char board[ROWS][COLS], char piece) {
    // Check horizontal
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j <= COLS - 4; j++) {
            if (board[i][j] == piece &&
                board[i][j+1] == piece &&
                board[i][j+2] == piece &&
                board[i][j+3] == piece) {
                return 1;
            }
        }
    }

    // Check vertical
    for (int j = 0; j < COLS; j++) {
        for (int i = 0; i <= ROWS - 4; i++) {
            if (board[i][j] == piece &&
                board[i+1][j] == piece &&
                board[i+2][j] == piece &&
                board[i+3][j] == piece) {
                return 1;
            }
        }
    }

    // Check diagonal (bottom-left to top-right)
    for (int i = 0; i <= ROWS - 4; i++) {
        for (int j = 0; j <= COLS - 4; j++) {
            if (board[i][j] == piece &&
                board[i+1][j+1] == piece &&
                board[i+2][j+2] == piece &&
                board[i+3][j+3] == piece) {
                return 1;
            }
        }
    }

    // Check anti-diagonal (top-left to bottom-right)
    for (int i = 3; i < ROWS; i++) {
        for (int j = 0; j <= COLS - 4; j++) {
            if (board[i][j] == piece &&
                board[i-1][j+1] == piece &&
                board[i-2][j+2] == piece &&
                board[i-3][j+3] == piece) {
                return 1;
            }
        }
    }

    return 0;
}

bool is_board_full(char board[ROWS][COLS]) {
    // Check if the board is completely filled (no empty spaces left)
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            if (board[i][j] == ' ') {
                return false; // Found an empty space, board is not full
            }
        }
    }
    return true; // Board is full
}

int main() {
    char board[ROWS][COLS];
    int current_player = 1; // Player 1 starts
    int column;

    initialize_board(board);

    while (1) {
        display_board(board);

        printf("Player %d, enter column (1-7): ", current_player);
        scanf("%d", &column);
        column--;

        if (column < 0 || column >= COLS || is_column_full(board, column)) {
            printf("Invalid move. Try again.\n");
            continue;
        }

        // Drop piece into the board
        char piece = (current_player == 1) ? 'X' : 'O';
        drop_piece(board, column, piece);

        if (check_win(board, piece)) {
            display_board(board);
            printf("Player %d wins!\n", current_player);
            break;
        }

        if (is_board_full(board)) {
            display_board(board);
            printf("It's a draw!\n");
            break;
        }
        // Switch to the other player
        current_player = (current_player == 1) ? 2 : 1;
    }

    return 0;
}
