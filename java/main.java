import java.util.Scanner;

class ConnectFour {
    private static final int ROWS = 6;
    private static final int COLUMNS = 7;
    private static final char[] players = {'R', 'Y'};
    private char[][] board = new char[ROWS][COLUMNS];

    public ConnectFour() {
        for (int i = 0; i < ROWS; i++) {
            for (int j = 0; j < COLUMNS; j++) {
                board[i][j] = '.';
            }
        }
    }

    public void playGame() {
        Scanner scanner = new Scanner(System.in);
        int playerIndex = 0;

        while (true) {
            System.out.println("Current board:");
            printBoard();

            int col;
            while (true) {
                System.out.println("Player " + players[playerIndex] + "'s turn. Enter column (0-6): ");
                col = scanner.nextInt();
                if (col >= 0 && col < COLUMNS && board[0][col] == '.') {
                    break;
                }
                System.out.println("Column " + col + " is full or out of bounds, choose another.");
            }

            for (int row = ROWS - 1; row >= 0; row--) {
                if (board[row][col] == '.') {
                    board[row][col] = players[playerIndex];
                    break;
                }
            }

            if (isWinningMove(col)) {
                printBoard();
                System.out.println("Player " + players[playerIndex] + " wins!");
                break;
            }

            playerIndex = 1 - playerIndex;
        }
        scanner.close();
    }

    private boolean isWinningMove(int lastCol) {
        for (int row = 0; row < ROWS; row++) {
            if (board[row][lastCol] != '.') {
                char token = board[row][lastCol];
                if (checkDirection(row, lastCol, 1, 0, token) ||
                        checkDirection(row, lastCol, 0, 1, token) ||
                        checkDirection(row, lastCol, 1, -1, token) ||
                        checkDirection(row, lastCol, 1, 1, token)) {
                    return true;
                }
                break;
            }
        }
        return false;
    }

    private boolean checkDirection(int row, int col, int dRow, int dCol, char token) {
        int count = 1;
        int r = row + dRow;
        int c = col + dCol;
        while (r >= 0 && r < ROWS && c >= 0 && c < COLUMNS && board[r][c] == token) {
            count++;
            r += dRow;
            c += dCol;
        }
        r = row - dRow;
        c = col - dCol;
        while (r >= 0 && r < ROWS && c >= 0 && c < COLUMNS && board[r][c] == token) {
            count++;
            r -= dRow;
            c -= dCol;
        }
        return count >= 4;
    }

    private void printBoard() {
        for (int i = 0; i < ROWS; i++) {
            for (int j = 0; j < COLUMNS; j++) {
                System.out.print(board[i][j] + " ");
            }
            System.out.println();
        }
    }
}

public class Main {
    public static void main(String[] args) {
        ConnectFour game = new ConnectFour();
        game.playGame();
    }
}
