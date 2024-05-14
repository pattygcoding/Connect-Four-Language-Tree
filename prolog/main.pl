% Connect Four game in Prolog

% Define the board dimensions
num_rows(6).
num_cols(7).

% Initialize an empty board
init_board(Board) :-
    num_rows(Rows),
    num_cols(Cols),
    length(Row, Cols), maplist(=(empty), Row),
    length(Board, Rows), maplist(=(Row), Board).

% Print the board
print_board(Board) :-
    num_cols(Cols),
    forall(member(Row, Board), (
        forall(member(Cell, Row), (
            ( Cell == empty -> write('. ') ; write(Cell), write(' ') )
        )),
        nl
    )),
    forall(between(1, Cols, Col), (write(Col), write(' '))),
    nl.

% Drop a piece into a column
drop_piece(Board, Col, Player, NewBoard) :-
    num_rows(Rows),
    nth1(Row, Board, Line),
    nth1(Col, Line, empty),
    \+ (Row \= Rows, Row1 is Row + 1, nth1(Row1, Board, Line1), nth1(Col, Line1, empty)),
    replace(Board, Row, Col, Player, NewBoard).

replace(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_column(OldRow, Col, Player, NewRow),
    replace_row(Board, Row, NewRow, NewBoard).

replace_row([_|Rest], 1, NewRow, [NewRow|Rest]).
replace_row([Row|Rest], N, NewRow, [Row|NewRest]) :-
    N > 1, N1 is N - 1, replace_row(Rest, N1, NewRow, NewRest).

replace_column([_|Rest], 1, Player, [Player|Rest]).
replace_column([Cell|Rest], N, Player, [Cell|NewRest]) :-
    N > 1, N1 is N - 1, replace_column(Rest, N1, Player, NewRest).

% Check for a winning condition
check_winner(Board, Player) :-
    horizontal_win(Board, Player) ;
    vertical_win(Board, Player) ;
    diagonal_win(Board, Player).

horizontal_win(Board, Player) :-
    member(Row, Board),
    consecutive_four(Row, Player).

vertical_win(Board, Player) :-
    transpose(Board, Transposed),
    horizontal_win(Transposed, Player).

diagonal_win(Board, Player) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, Player).

consecutive_four([X,X,X,X|_], X).
consecutive_four([_|T], X) :- consecutive_four(T, X).

diagonal(Board, Diagonals) :-
    findall(Diag, diagonal_line(Board, Diag), Diagonals).

diagonal_line(Board, Diagonal) :-
    num_rows(Rows),
    num_cols(Cols),
    between(-Rows, Cols, Offset),
    findall(Cell, (between(1, Rows, Row), Col is Row + Offset, Col > 0, Col =< Cols, nth1(Row, Board, Line), nth1(Col, Line, Cell)), Diagonal).

% Transpose the board
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% Alternate turns between players
next_player(x, o).
next_player(o, x).

% Main game loop
play_game(Board, Player) :-
    print_board(Board),
    format('Player ~w, choose a column (1-7): ', [Player]),
    read(Col),
    (   drop_piece(Board, Col, Player, NewBoard) ->
        (   check_winner(NewBoard, Player) ->
            print_board(NewBoard),
            format('Player ~w wins!~n', [Player])
        ;   next_player(Player, NextPlayer),
            play_game(NewBoard, NextPlayer)
        )
    ;   format('Invalid move. Try again.~n'),
        play_game(Board, Player)
    ).

% Start the game
start_game :-
    init_board(Board),
    play_game(Board, x).

% Start the game automatically when the script is loaded
:- initialization(start_game).
