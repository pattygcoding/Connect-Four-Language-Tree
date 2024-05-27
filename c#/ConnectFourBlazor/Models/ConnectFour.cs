namespace ConnectFourBlazor.Models
{
    public class ConnectFour
    {
        public int[,] Board { get; private set; }
        public int CurrentPlayer { get; private set; }
        public bool GameOver { get; private set; }
        public string Message { get; private set; }

        public ConnectFour()
        {
            Board = new int[6, 7];
            CurrentPlayer = 1;
            GameOver = false;
            Message = "Player 1's turn";
        }

        public bool DropDisc(int column)
        {
            if (GameOver || column < 0 || column > 6)
                return false;

            for (int row = 5; row >= 0; row--)
            {
                if (Board[row, column] == 0)
                {
                    Board[row, column] = CurrentPlayer;
                    if (CheckForWin(CurrentPlayer))
                    {
                        GameOver = true;
                        Message = $"Player {CurrentPlayer} wins!";
                    }
                    else
                    {
                        CurrentPlayer = CurrentPlayer == 1 ? 2 : 1;
                        Message = $"Player {CurrentPlayer}'s turn";
                    }
                    return true;
                }
            }
            return false;
        }

        private bool CheckForWin(int player)
        {
            for (int row = 0; row < 6; row++)
            {
                for (int col = 0; col < 7; col++)
                {
                    if (CheckDirection(row, col, 1, 0, player) ||
                        CheckDirection(row, col, 0, 1, player) ||
                        CheckDirection(row, col, 1, 1, player) ||
                        CheckDirection(row, col, 1, -1, player))
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        private bool CheckDirection(int row, int col, int rowDir, int colDir, int player)
        {
            int count = 0;
            for (int i = 0; i < 4; i++)
            {
                int newRow = row + i * rowDir;
                int newCol = col + i * colDir;

                if (newRow < 0 || newRow >= 6 || newCol < 0 || newCol >= 7)
                    return false;

                if (Board[newRow, newCol] == player)
                    count++;
                else
                    break;
            }
            return count == 4;
        }
    }
}
