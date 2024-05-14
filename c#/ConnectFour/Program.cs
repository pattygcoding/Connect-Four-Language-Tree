using System;

class Connect4
{
    private const int Rows = 6;
    private const int Cols = 7;
    private char[,] board;
    private int currentPlayer;

    public Connect4()
    {
        board = new char[Rows, Cols];
        InitializeBoard();
        currentPlayer = 1;
    }

    private void InitializeBoard()
    {
        for (int row = 0; row < Rows; row++)
        {
            for (int col = 0; col < Cols; col++)
            {
                board[row, col] = ' ';
            }
        }
    }

    private void DisplayBoard()
    {
        Console.WriteLine();
        Console.WriteLine("  1   2   3   4   5   6   7");
        for (int row = 0; row < Rows; row++)
        {
            for (int col = 0; col < Cols; col++)
            {
                Console.Write($"| {board[row, col]} ");
            }
            Console.WriteLine("|");
        }
        Console.WriteLine(new string('-', 4 * Cols + 1));
    }

    private bool DropPiece(int col)
    {
        if (col < 0 || col >= Cols || board[0, col] != ' ')
        {
            Console.WriteLine("Invalid move. Please try again.");
            return false;
        }

        for (int row = Rows - 1; row >= 0; row--)
        {
            if (board[row, col] == ' ')
            {
                board[row, col] = (currentPlayer == 1) ? 'X' : 'O';
                return true;
            }
        }

        return false;
    }

    private bool CheckWin()
    {
        char playerPiece = (currentPlayer == 1) ? 'X' : 'O';

        // Check horizontal
        for (int row = 0; row < Rows; row++)
        {
            for (int col = 0; col <= Cols - 4; col++)
            {
                if (board[row, col] == playerPiece &&
                    board[row, col + 1] == playerPiece &&
                    board[row, col + 2] == playerPiece &&
                    board[row, col + 3] == playerPiece)
                {
                    return true;
                }
            }
        }

        // Check vertical
        for (int col = 0; col < Cols; col++)
        {
            for (int row = 0; row <= Rows - 4; row++)
            {
                if (board[row, col] == playerPiece &&
                    board[row + 1, col] == playerPiece &&
                    board[row + 2, col] == playerPiece &&
                    board[row + 3, col] == playerPiece)
                {
                    return true;
                }
            }
        }

        // Check diagonal (top-left to bottom-right)
        for (int row = 0; row <= Rows - 4; row++)
        {
            for (int col = 0; col <= Cols - 4; col++)
            {
                if (board[row, col] == playerPiece &&
                    board[row + 1, col + 1] == playerPiece &&
                    board[row + 2, col + 2] == playerPiece &&
                    board[row + 3, col + 3] == playerPiece)
                {
                    return true;
                }
            }
        }

        // Check anti-diagonal (bottom-left to top-right)
        for (int row = Rows - 1; row >= 3; row--)
        {
            for (int col = 0; col <= Cols - 4; col++)
            {
                if (board[row, col] == playerPiece &&
                    board[row - 1, col + 1] == playerPiece &&
                    board[row - 2, col + 2] == playerPiece &&
                    board[row - 3, col + 3] == playerPiece)
                {
                    return true;
                }
            }
        }

        return false;
    }

    public void Play()
    {
        DisplayBoard();

        while (true)
        {
            Console.Write($"Player {currentPlayer}, enter column (1-7): ");
            if (!int.TryParse(Console.ReadLine(), out int col) || col < 1 || col > Cols)
            {
                Console.WriteLine("Invalid input. Please enter a number between 1 and 7.");
                continue;
            }

            if (DropPiece(col - 1))
            {
                if (CheckWin())
                {
                    DisplayBoard();
                    Console.WriteLine($"Player {currentPlayer} wins!");
                    break;
                }

                if (IsBoardFull())
                {
                    DisplayBoard();
                    Console.WriteLine("It's a draw!");
                    break;
                }

                currentPlayer = (currentPlayer == 1) ? 2 : 1;
                DisplayBoard();
            }
        }
    }

    private bool IsBoardFull()
    {
        for (int col = 0; col < Cols; col++)
        {
            if (board[0, col] == ' ')
            {
                return false;
            }
        }
        return true;
    }

    static void Main()
    {
        Connect4 game = new Connect4();
        game.Play();
    }
}
