#include <iostream>
#include <vector>
#include <string>

// using namespace std works here too, but this is better coding
using std::cin, std::cout, std::vector;

class Connect4 {
private:
    const int ROWS = 6;
    const int COLS = 7;
    vector<vector<char>> board;
    int currentPlayer;

public:
    Connect4() : board(ROWS, vector<char>(COLS, ' ')), currentPlayer(1) {}

    // Display the current state of the board
    void displayBoard() const {
        cout << "\n";
        for (int i = 0; i < ROWS; i++) {
            cout << "| ";
            for (int j = 0; j < COLS; j++) {
                cout << board[i][j] << " | ";
            }
            cout << "\n";
        }
        cout << "-----------------------------\n";
        cout << "| 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n";
    }

    // Drop a piece into the specified column
    bool dropPiece(int col) {
        if (col < 0 || col >= COLS) {
            cout << "Invalid column. Please try again.\n";
            return false;
        }

        for (int i = ROWS - 1; i >= 0; i--) {
            if (board[i][col] == ' ') {
                board[i][col] = (currentPlayer == 1) ? 'X' : 'O';
                return true;
            }
        }

        cout << "Column is full. Please choose another column.\n";
        return false;
    }

    // Check if the last move resulted in a win
    bool checkWin() const {
            char piece = (currentPlayer == 1) ? 'X' : 'O';

            // Check horizontal
            for (int i = 0; i < ROWS; i++) {
                for (int j = 0; j <= COLS - 4; j++) {
                    if (board[i][j] == piece &&
                        board[i][j+1] == piece &&
                        board[i][j+2] == piece &&
                        board[i][j+3] == piece) {
                        return true;
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
                        return true;
                    }
                }
            }

            // Check diagonal (both directions)
            for (int i = 0; i <= ROWS - 4; i++) {
                for (int j = 0; j <= COLS - 4; j++) {
                    if (board[i][j] == piece &&
                        board[i+1][j+1] == piece &&
                        board[i+2][j+2] == piece &&
                        board[i+3][j+3] == piece) {
                        return true;
                    }
                }
            }

            // Check anti-diagonal (both directions)
            for (int i = 0; i <= ROWS - 4; i++) {
                for (int j = COLS - 1; j >= 3; j--) {
                    if (board[i][j] == piece &&
                        board[i+1][j-1] == piece &&
                        board[i+2][j-2] == piece &&
                        board[i+3][j-3] == piece) {
                        return true;
                    }
                }
            }

            return false;
        }

    // Check if the board is full (draw condition)
    bool boardFull() const {
        for (int i = 0; i < ROWS; i++) {
            for (int j = 0; j < COLS; j++) {
                if (board[i][j] == ' ') {
                    return false;
                }
            }
        }
        return true;
    }

    // Play the game
    void play() {
        displayBoard();

        while (true) {
            int col;
            cout << "Player " << currentPlayer << ", enter column (1-7): ";
            cin >> col;
            col--; // Convert to zero-based index

            if (dropPiece(col)) {
                if (checkWin()) {
                    displayBoard();
                    cout << "Player " << currentPlayer << " wins!\n";
                    break;
                }

                if (boardFull()) {
                    displayBoard();                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                    cout << "It's a draw!\n";
                    break;
                }

                // Switch player
                currentPlayer = (currentPlayer == 1) ? 2 : 1;
                displayBoard();
            }
        }
    }
};

int main() {
    Connect4 game;
    game.play();

    return 0;
}
