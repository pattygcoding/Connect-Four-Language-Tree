-module(connect_four).
-export([start_game/0, play/1]).

% Constants
-define(ROWS, 6).
-define(COLS, 7).
-define(EMPTY, 0).
-define(PLAYER1, 1).
-define(PLAYER2, 2).

% Initialize the board
init_board() ->
    lists:duplicate(?ROWS, lists:duplicate(?COLS, ?EMPTY)).

% Display the board
display_board(Board) ->
    lists:foreach(fun(Row) ->
        io:format("~p~n", [Row])
    end, Board).

% Start the game
start_game() ->
    Board = init_board(),
    display_board(Board),
    play(Board, ?PLAYER1).

% Play the game
play(Board, Player) ->
    io:format("Player ~p, enter column (1-7): ", [Player]),
    {ok, ColumnStr} = io:get_line(""),
    {ok, Column} = string:to_integer(string:trim(ColumnStr)),
    case make_move(Board, Column - 1, Player) of
        {ok, NewBoard} ->
            display_board(NewBoard),
            case check_winner(NewBoard, Player) of
                true ->
                    io:format("Player ~p wins!~n", [Player]);
                false ->
                    NextPlayer = if Player =:= ?PLAYER1 -> ?PLAYER2; true -> ?PLAYER1 end,
                    play(NewBoard, NextPlayer)
            end;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            play(Board, Player)
    end.

% Make a move
make_move(Board, Col, Player) when Col >= 0, Col < ?COLS ->
    make_move(Board, Col, Player, ?ROWS - 1);
make_move(_, _, _) ->
    {error, invalid_column}.

make_move(Board, Col, Player, Row) when Row >= 0 ->
    case lists:nth(Row + 1, lists:nth(Col + 1, Board)) of
        ?EMPTY ->
            NewRow = lists:substitute(Row + 1, Board, set_elem(Col + 1, lists:nth(Row + 1, Board), Player)),
            {ok, set_elem(Row + 1, Board, NewRow)};
        _ ->
            make_move(Board, Col, Player, Row - 1)
    end;
make_move(_, _, _, _) ->
    {error, column_full}.

set_elem(Index, List, Elem) ->
    lists:substitute(Index, List, Elem).

% Check for winner
check_winner(Board, Player) ->
    check_rows(Board, Player) orelse
    check_columns(Board, Player) orelse
    check_diagonals(Board, Player).

check_rows(Board, Player) ->
    lists:any(fun(Row) ->
        check_line(Row, Player)
    end, Board).

check_columns(Board, Player) ->
    Columns = lists:transpose(Board),
    lists:any(fun(Column) ->
        check_line(Column, Player)
    end, Columns).

check_diagonals(Board, Player) ->
    Diagonals = get_diagonals(Board),
    lists:any(fun(Diagonal) ->
        check_line(Diagonal, Player)
    end, Diagonals).

check_line(Line, Player) ->
    lists:any(fun(SubList) ->
        length(SubList) >= 4 andalso lists:all(fun(Elem) -> Elem =:= Player end, SubList)
    end, lists:sublist(Line, 4)).

get_diagonals(Board) ->
    Diagonals1 = [lists:nth(N + K, lists:nth(K + 1, Board)) || K <- lists:seq(0, ?ROWS - 1), N <- lists:seq(1, ?COLS)],
    Diagonals2 = [lists:nth(K + 1, lists:nth(N + K, Board)) || K <- lists:seq(0, ?ROWS - 1), N <- lists:seq(1, ?COLS)],
    Diagonals1 ++ Diagonals2.
