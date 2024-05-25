using System;
using Microsoft.EntityFrameworkCore;

class ConnectFour
{
    const int Rows = 6;
    const int Columns = 7;
    char[,] board = new char[Rows, Columns];
    GameContext db;

    public ConnectFour()
    {
        db = new GameContext();
        InitializeBoard();
    }

    public void PlayGame()
    {
        char currentPlayer = 'X';
        bool gameWon = false;

        LoadGameState();

        while (!gameWon)
        {
            PrintBoard();
            Console.WriteLine($"Player {currentPlayer}'s turn. Choose a column (0-{Columns - 1}): ");
            int column;
            while (!int.TryParse(Console.ReadLine(), out column) || column < 0 || column >= Columns || board[0, column] != '.')
            {
                Console.WriteLine($"Invalid column. Choose a column (0-{Columns - 1}): ");
            }

            int row = DropDisc(currentPlayer, column);
            gameWon = CheckForWin(currentPlayer, row, column);

            if (gameWon)
            {
                PrintBoard();
                Console.WriteLine($"Player {currentPlayer} wins!");
                SaveGameState(currentPlayer);
            }
            else
            {
                currentPlayer = currentPlayer == 'X' ? 'O' : 'X';
                SaveGameState(currentPlayer);
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
        // Check vertical, horizontal, and two diagonals
        return CheckDirection(player, row, column, 1, 0) || // Vertical
               CheckDirection(player, row, column, 0, 1) || // Horizontal
               CheckDirection(player, row, column, 1, 1) || // Diagonal /
               CheckDirection(player, row, column, 1, -1);  // Diagonal \
    }

    bool CheckDirection(char player, int row, int column, int deltaRow, int deltaColumn)
    {
        int count = 1;
        count += CountInDirection(player, row, column, deltaRow, deltaColumn);
        count += CountInDirection(player, row, column, -deltaRow, -deltaColumn);
        return count >= 4;
    }

    int CountInDirection(char player, int row, int column, int deltaRow, int deltaColumn)
    {
        int count = 0;
        int currentRow = row + deltaRow;
        int currentColumn = column + deltaColumn;

        while (currentRow >= 0 && currentRow < Rows && currentColumn >= 0 && currentColumn < Columns && board[currentRow, currentColumn] == player)
        {
            count++;
            currentRow += deltaRow;
            currentColumn += deltaColumn;
        }

        return count;
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

    void SaveGameState(char currentPlayer)
    {
        var gameState = new GameState
        {
            Board = string.Join(",", board.Cast<char>()),
            CurrentPlayer = currentPlayer
        };

        db.GameStates.Add(gameState);
        db.SaveChanges();
    }

    void LoadGameState()
    {
        var gameState = db.GameStates.OrderByDescending(g => g.Id).FirstOrDefault();
        if (gameState != null)
        {
            var boardData = gameState.Board.Split(',');
            for (int i = 0; i < boardData.Length; i++)
            {
                board[i / Columns, i % Columns] = boardData[i][0];
            }
        }
    }

    static void Main()
    {
        var game = new ConnectFour();
        game.PlayGame();
    }
}
