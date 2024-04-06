#import <Foundation/Foundation.h>

// Constants
#define ROWS 6
#define COLS 7
#define EMPTY ' '
#define PLAYER1 'X'
#define PLAYER2 'O'

// Game state
char board[ROWS][COLS];

// Initialize the game board
void initializeBoard() {
    for (int row = 0; row < ROWS; row++) {
        for (int col = 0; col < COLS; col++) {
            board[row][col] = EMPTY;
        }
    }
}

// Display the game board
void displayBoard() {
    printf(" 1 2 3 4 5 6 7\n");
    for (int row = 0; row < ROWS; row++) {
        printf("|");
        for (int col = 0; col < COLS; col++) {
            printf("%c|", board[row][col]);
        }
        printf("\n");
    }
    printf("---------------\n");
}

// Drop a piece into a column
BOOL dropPiece(int col, char player) {
    if (col < 0 || col >= COLS) {
        return NO; // Invalid column
    }

    for (int row = ROWS - 1; row >= 0; row--) {
        if (board[row][col] == EMPTY) {
            board[row][col] = player;
            return YES;
        }
    }

    return NO; // Column is full
}

// Check for a win condition
BOOL checkWin(char player) {
    // Check horizontal
    for (int row = 0; row < ROWS; row++) {
        for (int col = 0; col <= COLS - 4; col++) {
            if (board[row][col] == player &&
                board[row][col + 1] == player &&
                board[row][col + 2] == player &&
                board[row][col + 3] == player) {
                return YES;
            }
        }
    }

    // Check vertical
    for (int col = 0; col < COLS; col++) {
        for (int row = 0; row <= ROWS - 4; row++) {
            if (board[row][col] == player &&
                board[row + 1][col] == player &&
                board[row + 2][col] == player &&
                board[row + 3][col] == player) {
                return YES;
            }
        }
    }

    // Check diagonal (top-left to bottom-right)
    for (int row = 0; row <= ROWS - 4; row++) {
        for (int col = 0; col <= COLS - 4; col++) {
            if (board[row][col] == player &&
                board[row + 1][col + 1] == player &&
                board[row + 2][col + 2] == player &&
                board[row + 3][col + 3] == player) {
                return YES;
            }
        }
    }

    // Check diagonal (top-right to bottom-left)
    for (int row = 0; row <= ROWS - 4; row++) {
        for (int col = 3; col < COLS; col++) {
            if (board[row][col] == player &&
                board[row + 1][col - 1] == player &&
                board[row + 2][col - 2] == player &&
                board[row + 3][col - 3] == player) {
                return YES;
            }
        }
    }

    return NO;
}

// Main game loop
void playConnectFour() {
    initializeBoard();
    char currentPlayer = PLAYER1;
    BOOL gameOver = NO;

    while (!gameOver) {
        displayBoard();

        printf("Player %c's turn. Enter column (1-7): ", currentPlayer);
        int col;
        scanf("%d", &col);
        col--; // Adjust column index (convert 1-based to 0-based)

        if (dropPiece(col, currentPlayer)) {
            if (checkWin(currentPlayer)) {
                displayBoard();
                printf("Player %c wins!\n", currentPlayer);
                gameOver = YES;
            } else if ([[NSData dataWithBytes:board length:sizeof(board)] isEqualToData:[NSData dataWithBytes:board length:sizeof(board)]]) {
                displayBoard();
                printf("It's a draw! The board is full.\n");
                gameOver = YES;
            } else {
                currentPlayer = (currentPlayer == PLAYER1) ? PLAYER2 : PLAYER1;
            }
        } else {
            printf("Invalid move. Please choose another column.\n");
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        playConnectFour();
    }
    return 0;
}
