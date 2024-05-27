using System;
using System.Collections.Generic;
using System.Linq;

class ConnectFour
{
    const int Rows = 6;
    const int Columns = 7;
    char[,] board = new char[Rows, Columns];

    public ConnectFour()
    {
        InitializeBoard();
    }

    public void PlayGame()
    {
        char currentPlayer = 'X';
        bool gameWon = false;

        while (!gameWon)
        {
            PrintBoard();
            Console.WriteLine($"Player {currentPlayer}'s turn. Choose a column (1-{Columns}): ");
            int column;
            while (!int.TryParse(Console.ReadLine(), out column) || column < 1 || column > Columns || board[0, column - 1] != '.')
            {
                Console.WriteLine($"Invalid column. Choose a column (1-{Columns}): ");
            }

            int row = DropDisc(currentPlayer, column - 1);
            gameWon = CheckForWin(currentPlayer, row, column - 1);

            if (gameWon)
            {
                PrintBoard();
                Console.WriteLine($"Player {currentPlayer} wins!");
            }
            else
            {
                currentPlayer = currentPlayer == 'X' ? 'O' : 'X';
            }
        }
    }

    void InitializeBoard()
    {
        for (int row = 0; row < Rows; row++)
        {
            for (int col = 0; col < Columns; col++)
            {
                board[row, col] = '.';
            }
        }
    }

    int DropDisc(char player, int column)
    {
        for (int row = Rows - 1; row >= 0; row--)
        {
            if (board[row, column] == '.')
            {
                board[row, column] = player;
                return row;
            }
        }
        return -1;
    }

    bool CheckForWin(char player, int row, int column)
    {
        return CheckDirection(player, row, column, 1, 0) || // Vertical
               CheckDirection(player, row, column, 0, 1) || // Horizontal
               CheckDirection(player, row, column, 1, 1) || // Diagonal /
               CheckDirection(player, row, column, 1, -1);  // Diagonal \
    }

    bool CheckDirection(char player, int row, int column, int deltaRow, int deltaColumn)
    {
        return CountInDirection(player, row, column, deltaRow, deltaColumn) +
               CountInDirection(player, row, column, -deltaRow, -deltaColumn) - 1 >= 4;
    }

    int CountInDirection(char player, int row, int column, int deltaRow, int deltaColumn)
    {
        var positions = Enumerable.Range(0, 4)
                                  .Select(i => new { Row = row + i * deltaRow, Col = column + i * deltaColumn })
                                  .Where(pos => pos.Row >= 0 && pos.Row < Rows && pos.Col >= 0 && pos.Col < Columns)
                                  .TakeWhile(pos => board[pos.Row, pos.Col] == player)
                                  .Count();

        return positions;
    }

    void PrintBoard()
    {
        for (int row = 0; row < Rows; row++)
        {
            for (int col = 0; col < Columns; col++)
            {
                Console.Write(board[row, col] + " ");
            }
            Console.WriteLine();
        }
        Console.WriteLine(new string('-', Columns * 2));
    }

    static void Main()
    {
        var game = new ConnectFour();
        game.PlayGame();
    }
}
