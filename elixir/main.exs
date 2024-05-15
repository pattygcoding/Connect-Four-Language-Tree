defmodule Main do
  @rows 6
  @cols 7

  defstruct board: :lists.duplicate(@rows, :lists.duplicate(@cols, ".")), current_player: "R"

  def start_game do
    %Main{}
    |> play()
  end

  defp play(game) do
    game
    |> print_board()
    |> get_player_input()
    |> drop_disc()
    |> check_winner()
    |> switch_player()
    |> play()
  end

  defp print_board(%Main{board: board}) do
    Enum.each(board, fn row ->
      IO.puts Enum.join(row, " ")
    end)
    game
  end

  defp get_player_input(%Main{current_player: player} = game) do
    IO.puts "Player #{player}, choose a column (0-#{@cols - 1}):"
    col = String.trim(IO.gets("")) |> String.to_integer()
    {col, game}
  end

  defp drop_disc({col, %Main{board: board, current_player: player} = game}) do
    {new_board, valid_move} = drop_in_column(board, col, player)
    if valid_move do
      %{game | board: new_board}
    else
      IO.puts "Invalid move. Try again."
      get_player_input(game) |> drop_disc()
    end
  end

  defp drop_in_column(board, col, player) do
    updated_board = Enum.with_index(board) |> Enum.reduce_while(board, fn {row, row_index}, acc ->
      if Enum.at(row, col) == "." do
        new_row = List.replace_at(row, col, player)
        {:halt, List.replace_at(acc, row_index, new_row)}
      else
        {:cont, acc}
      end
    end)
    {updated_board, updated_board != board}
  end

  defp check_winner(%Main{board: board, current_player: player} = game) do
    if winner?(board, player) do
      print_board(game)
      IO.puts "Player #{player} wins!"
      System.halt(0)
    else
      game
    end
  end

  defp winner?(board, player) do
    Enum.any?(for row <- 0..(@rows - 1), col <- 0..(@cols - 1), do: {row, col}, fn {row, col} ->
      check_direction(board, row, col, player, 1, 0) ||
      check_direction(board, row, col, player, 0, 1) ||
      check_direction(board, row, col, player, 1, 1) ||
      check_direction(board, row, col, player, 1, -1)
    end)
  end

  defp check_direction(board, row, col, player, row_dir, col_dir) do
    Enum.reduce_while(0..3, true, fn i, acc ->
      r = row + i * row_dir
      c = col + i * col_dir
      if r < @rows and r >= 0 and c < @cols and c >= 0 and Enum.at(Enum.at(board, r), c) == player do
        {:cont, true}
      else
        {:halt, false}
      end
    end)
  end

  defp switch_player(%Main{current_player: player} = game) do
    new_player = if player == "R", do: "Y", else: "R"
    %{game | current_player: new_player}
  end
end

Main.start_game()
