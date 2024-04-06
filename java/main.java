import java.util.Scanner;

public class Main {
    private static final int ROWS = 6;
    private static final int COLS = 7;
    private static final char EMPTY = ' ';
    private static final char PLAYER1 = 'X';
    private static final char PLAYER2 = 'O';

    private char[][] board;
    private boolean player1Turn;

    public Main() {
        board = new char[ROWS][COLS];
        player1Turn = true;
        initializeBoard();
    }

    private void initializeBoard() {
        for (int row = 0; row < ROWS; row++) {
            for (int col = 0; col < COLS; col++) {
                board[row][col] = EMPTY;
            }
        }
    }

    private void displayBoard() {
        System.out.println(" 1 2 3 4 5 6 7");
        for (int row = 0; row < ROWS; row++) {
            System.out.print("|");
            for (int col = 0; col < COLS; col++) {
                System.out.print(board[row][col] + "|");
            }
            System.out.println();
        }
        System.out.println("---------------");
    }

    private boolean dropPiece(int col) {
        for (int row = ROWS - 1; row >= 0; row--) {
            if (board[row][col] == EMPTY) {
                board[row][col] = player1Turn ? PLAYER1 : PLAYER2;
                return true;
            }
        }
        return false; // Column is full
    }

    private boolean checkWin(int row, int col) {
        char player = board[row][col];

        // Check vertical
        int count = 0;
        for (int r = row; r < ROWS; r++) {
            if (board[r][col] == player) {
                count++;
            } else {
                break;
            }
        }
        if (count >= 4) {
            return true;
        }

        // Check horizontal
        count = 0;
        for (int c = 0; c < COLS; c++) {
            if (board[row][c] == player) {
                count++;
            } else {
                count = 0;
            }
            if (count >= 4) {
                return true;
            }
        }

        // Check diagonal (bottom-left to top-right)
        count = 0;
        for (int r = row, c = col; r < ROWS && c < COLS; r++, c++) {
            if (board[r][c] == player) {
                count++;
            } else {
                count = 0;
            }
            if (count >= 4) {
                return true;
            }
        }

        // Check diagonal (top-left to bottom-right)
        count = 0;
        for (int r = row, c = col; r >= 0 && c < COLS; r--, c++) {
            if (board[r][c] == player) {
                count++;
            } else {
                count = 0;
            }
            if (count >= 4) {
                return true;
            }
        }

        return false;
    }

    public void play() {
        Scanner scanner = new Scanner(System.in);

        while (true) {
            displayBoard();

            char currentPlayer = player1Turn ? PLAYER1 : PLAYER2;
            System.out.println("Player " + currentPlayer + "'s turn.");

            System.out.print("Enter column (1-7): ");
            int col = scanner.nextInt() - 1;

            if (col < 0 || col >= COLS || !dropPiece(col)) {
                System.out.println("Invalid move. Please try again.");
                continue;
            }

            if (checkWin(ROWS - 1, col)) {
                displayBoard();
                System.out.println("Player " + currentPlayer + " wins!");
                break;
            }

            player1Turn = !player1Turn;
        }

        scanner.close();
    }

    public static void main(String[] args) {
        Main connectFour = new Main();
        connectFour.play();
    }
}
