with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Connect_Four is
   Board : array (1 .. 6, 1 .. 7) of Character := (others => (others => '.'));
   Current_Player : Character := 'R';

   procedure Print_Board is
   begin
      for I in 1 .. 6 loop
         for J in 1 .. 7 loop
            Put(Board(I, J));
            Put(' ');
         end loop;
         New_Line;
      end loop;
   end Print_Board;

   function Drop_Disc(Col : Integer) return Boolean is
   begin
      for I in reverse 1 .. 6 loop
         if Board(I, Col) = '.' then
            Board(I, Col) := Current_Player;
            return True;
         end if;
      end loop;
      return False;
   end Drop_Disc;

   function Check_Direction(Row, Col, Row_Dir, Col_Dir : Integer) return Boolean is
      Count : Integer := 0;
   begin
      for I in -3 .. 3 loop
         declare
            R : Integer := Row + I * Row_Dir;
            C : Integer := Col + I * Col_Dir;
         begin
            if R >= 1 and R <= 6 and C >= 1 and C <= 7 and Board(R, C) = Current_Player then
               Count := Count + 1;
               if Count = 4 then
                  return True;
               end if;
            else
               Count := 0;
            end if;
         end;
      end loop;
      return False;
   end Check_Direction;

   function Is_Winning_Move(Row, Col : Integer) return Boolean is
   begin
      return Check_Direction(Row, Col, 1, 0) or -- Horizontal
             Check_Direction(Row, Col, 0, 1) or -- Vertical
             Check_Direction(Row, Col, 1, 1) or -- Diagonal \
             Check_Direction(Row, Col, 1, -1);  -- Diagonal /
   end Is_Winning_Move;

   procedure Switch_Player is
   begin
      if Current_Player = 'R' then
         Current_Player := 'Y';
      else
         Current_Player := 'R';
      end if;
   end Switch_Player;

begin
   loop
      Print_Board;
      Put("Player ");
      Put(Current_Player);
      Put_Line(", choose a column (1-7): ");
      declare
         Col : Integer;
      begin
         Get(Col);
         if Col < 1 or else Col > 7 or else not Drop_Disc(Col) then
            Put_Line("Invalid move. Try again.");
            next;
         end if;
      end;
      declare
         Row : Integer := 1;
      begin
         while Board(Row, Col) /= Current_Player loop
            Row := Row + 1;
         end loop;
         if Is_Winning_Move(Row, Col) then
            Print_Board;
            Put("Player ");
            Put(Current_Player);
            Put_Line(" wins!");
            exit;
         end if;
      end;
      Switch_Player;
   end loop;
end Connect_Four;
