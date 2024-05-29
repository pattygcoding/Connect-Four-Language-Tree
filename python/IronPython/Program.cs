using System;
using IronPython.Hosting;
using Microsoft.Scripting.Hosting;

class Program
{
    static void Main(string[] args)
    {
        ScriptEngine engine = Python.CreateEngine();
        ScriptScope scope = engine.CreateScope();

        engine.ExecuteFile("connect_four.py", scope);

        dynamic connectFourClass = scope.GetVariable("ConnectFour");
        dynamic game = connectFourClass();

        while (true)
        {
            game.print_board();
            Console.WriteLine($"Player {game.current_player}, choose a column (0-{game.COLS - 1}): ");
            int col = Convert.ToInt32(Console.ReadLine());

            if (game.drop_piece(col))
            {
                if (game.check_winner())
                {
                    game.print_board();
                    Console.WriteLine($"Player {game.current_player} wins!");
                    break;
                }
                game.switch_player();
            }
            else
            {
                Console.WriteLine("Column is full! Choose another one.");
            }
        }
    }
}
