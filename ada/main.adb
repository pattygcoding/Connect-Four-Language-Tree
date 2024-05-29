with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   Board_Size : constant Integer := 6;
   Column_Size : constant Integer := 7;
   type Board_Type is array (1 .. Board_Size, 1 .. Column_Size) of Character;
   Board : Board_Type := (others => (others => ' '));

   Player1 : constant Character := 'X';
   Player2 : constant Character := 'O';
   Empty : constant Character := ' ';

   procedure Initialize_Board is
   begin
      for I in 1 .. Board_Size loop
         for J in 1 .. Column_Size loop
            Board(I, J) := Empty;
         end loop;
      end loop;
   end Initialize_Board;

   procedure Display_Board is
   begin
      for I in 1 .. Board_Size loop
         for J in 1 .. Column_Size loop
            Put(Board(I, J) & " ");
         end loop;
         New_Line;
      end loop;
      for J in 1 .. Column_Size loop
         Put(Integer'Image(J) & " ");
      end loop;
      New_Line;
   end Display_Board;

   function Is_Column_Full(Column : Integer) return Boolean is
   begin
      return Board(1, Column) /= Empty;
   end Is_Column_Full;

   function Get_Column return Integer is
      Column : Integer;
   begin
      loop
         Put("Enter column (1-7): ");
         Get(Column);
         if Column in 1 .. Column_Size and then not Is_Column_Full(Column) then
            return Column;
         else
            Put_Line("Invalid column. Try again.");
         end if;
      end loop;
   end Get_Column;

   procedure Drop_Token(Column : Integer; Token : Character) is
   begin
      for I in reverse 1 .. Board_Size loop
         if Board(I, Column) = Empty then
            Board(I, Column) := Token;
            exit;
         end if;
      end loop;
   end Drop_Token;

   function Check_Win(Token : Character) return Boolean is
   begin
      -- Check horizontal win
      for I in 1 .. Board_Size loop
         for J in 1 .. Column_Size - 3 loop
            if Board(I, J) = Token and then Board(I, J + 1) = Token and then
               Board(I, J + 2) = Token and then Board(I, J + 3) = Token then
               return True;
            end if;
         end loop;
      end loop;

      -- Check vertical win
      for I in 1 .. Board_Size - 3 loop
         for J in 1 .. Column_Size loop
            if Board(I, J) = Token and then Board(I + 1, J) = Token and then
               Board(I + 2, J) = Token and then Board(I + 3, J) = Token then
               return True;
            end if;
         end loop;
      end loop;

      -- Check diagonal win (bottom-left to top-right)
      for I in 4 .. Board_Size loop
         for J in 1 .. Column_Size - 3 loop
            if Board(I, J) = Token and then Board(I - 1, J + 1) = Token and then
               Board(I - 2, J + 2) = Token and then Board(I - 3, J + 3) = Token then
               return True;
            end if;
         end loop;
      end loop;

      -- Check diagonal win (top-left to bottom-right)
      for I in 1 .. Board_Size - 3 loop
         for J in 1 .. Column_Size - 3 loop
            if Board(I, J) = Token and then Board(I + 1, J + 1) = Token and then
               Board(I + 2, J + 2) = Token and then Board(I + 3, J + 3) = Token then
               return True;
            end if;
         end loop;
      end loop;

      return False;
   end Check_Win;

   procedure Play_Game is
      Current_Player : Character := Player1;
      Moves : Integer := 0;
      Max_Moves : constant Integer := Board_Size * Column_Size;
   begin
      Initialize_Board;
      loop
         Display_Board;
         Put_Line("Player " & Current_Player & "'s turn.");
         declare
            Column : Integer := Get_Column;
         begin
            Drop_Token(Column, Current_Player);
            Moves := Moves + 1;
            if Check_Win(Current_Player) then
               Display_Board;
               Put_Line("Player " & Current_Player & " wins!");
               exit;
            elsif Moves = Max_Moves then
               Display_Board;
               Put_Line("It's a draw!");
               exit;
            end if;
            if Current_Player = Player1 then
               Current_Player := Player2;
            else
               Current_Player := Player1;
            end if;
         end;
      end loop;
   end Play_Game;

begin
   Play_Game;
end Main;
