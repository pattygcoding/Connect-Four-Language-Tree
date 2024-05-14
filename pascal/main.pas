program ConnectFour;

uses crt;

const
  Rows = 6;
  Cols = 7;
  Empty = ' ';

type
  TBoard = array[1..Rows, 1..Cols] of char;
  TPlayer = (Player1, Player2);

var
  Board: TBoard;
  CurrentPlayer: TPlayer;

procedure InitializeBoard(var Board: TBoard);
var
  r, c: integer;
begin
  for r := 1 to Rows do
    for c := 1 to Cols do
      Board[r, c] := Empty;
end;

procedure PrintBoard(var Board: TBoard);
var
  r, c: integer;
begin
  clrscr;
  writeln(' 1 2 3 4 5 6 7');
  for r := 1 to Rows do
  begin
    for c := 1 to Cols do
    begin
      write('|', Board[r, c]);
    end;
    writeln('|');
  end;
  writeln('---------------');
end;

function DropPiece(var Board: TBoard; col: integer; player: char): boolean;
var
  r: integer;
begin
  DropPiece := False;
  for r := Rows downto 1 do
  begin
    if Board[r, col] = Empty then
    begin
      Board[r, col] := player;
      DropPiece := True;
      Exit;
    end;
  end;
end;

function CheckWinDirection(var Board: TBoard; r, c, dr, dc: integer; player: char): boolean;
var
  count, i: integer;
begin
  count := 0;
  for i := 0 to 3 do
  begin
    if (r + i * dr >= 1) and (r + i * dr <= Rows) and
       (c + i * dc >= 1) and (c + i * dc <= Cols) and
       (Board[r + i * dr, c + i * dc] = player) then
    begin
      Inc(count);
    end;
  end;
  CheckWinDirection := (count = 4);
end;

function CheckWinner(var Board: TBoard; player: char): boolean;
var
  r, c: integer;
begin
  for r := 1 to Rows do
  begin
    for c := 1 to Cols do
    begin
      if (Board[r, c] = player) and
         (CheckWinDirection(Board, r, c, 1, 0, player) or  // vertical
          CheckWinDirection(Board, r, c, 0, 1, player) or  // horizontal
          CheckWinDirection(Board, r, c, 1, 1, player) or  // diagonal down-right
          CheckWinDirection(Board, r, c, 1, -1, player)) then // diagonal down-left
      begin
        CheckWinner := True;
        Exit;
      end;
    end;
  end;
  CheckWinner := False;
end;

procedure SwitchPlayer(var CurrentPlayer: TPlayer);
begin
  if CurrentPlayer = Player1 then
    CurrentPlayer := Player2
  else
    CurrentPlayer := Player1;
end;

function PlayerChar(player: TPlayer): char;
begin
  if player = Player1 then
    PlayerChar := 'X'
  else
    PlayerChar := 'O';
end;

procedure PlayGame;
var
  col: integer;
  playerChar: char;
  validMove: boolean;
begin
  InitializeBoard(Board);
  CurrentPlayer := Player1;
  repeat
    PrintBoard(Board);
    playerChar := PlayerChar(CurrentPlayer);
    writeln('Player ', playerChar, ', choose a column (1-7): ');
    readln(col);
    if (col >= 1) and (col <= Cols) then
    begin
      validMove := DropPiece(Board, col, playerChar);
      if validMove then
      begin
        if CheckWinner(Board, playerChar) then
        begin
          PrintBoard(Board);
          writeln('Player ', playerChar, ' wins!');
          Exit;
        end
        else
        begin
          SwitchPlayer(CurrentPlayer);
        end;
      end
      else
        writeln('Column is full. Try another column.');
    end
    else
      writeln('Invalid column. Please choose a column between 1 and 7.');
  until False;
end;

begin
  PlayGame;
end.
