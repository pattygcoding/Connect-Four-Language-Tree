using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace ConnectFour
{
    public partial class MainWindow : Window
    {
        private int[,] board = new int[6, 7];
        private int currentPlayer = 1;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void DropDisc_Click(object sender, RoutedEventArgs e)
        {
            int column = Grid.GetColumn((UIElement)sender);

            for (int row = 5; row >= 0; row--)
            {
                if (board[row, column] == 0)
                {
                    board[row, column] = currentPlayer;

                    Ellipse disc = new Ellipse
                    {
                        Width = 50,
                        Height = 50,
                        Fill = currentPlayer == 1 ? Brushes.Red : Brushes.Yellow
                    };
                    Canvas.SetLeft(disc, column * 100 + 25);
                    Canvas.SetTop(disc, row * 100 + 25);
                    GameBoard.Children.Add(disc);

                    if (CheckForWin(currentPlayer))
                    {
                        MessageBox.Show($"Player {currentPlayer} wins!");
                    }

                    currentPlayer = currentPlayer == 1 ? 2 : 1;

                    break;
                }
            }
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

                if (board[newRow, newCol] == player)
                    count++;
                else
                    break;
            }
            return count == 4;
        }
    }
}
