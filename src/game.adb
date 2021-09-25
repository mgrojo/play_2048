
-- This isn't portable and raises a warning. It should be replaced by
-- the standard way in Ada 202x.
-- http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0144-1.txt?rev=1.13
with System.Random_Numbers;

package body Game is

   function Random_Int is new System.Random_Numbers.Random_Discrete(Integer);
   Generator : System.Random_Numbers.Generator;

   use type Ada.Calendar.Time;

   function Elapsed_Time (State : t_Board_State) return Duration is
      (Ada.Calendar.Clock - State.Start_Time);

   procedure Reset_Game (State : out t_Board_State) is
   begin
      State := (Board       => (others => (others => 0)),
                Blanks      => t_Cell_Count'Last,
                Score       => 0,
                Start_Time  => Ada.Calendar.Clock,
                Game_Status => Playing);
   end Reset_Game;

   procedure Restart_Game (State : out t_Board_State) is
   begin
      Reset_Game (State);
      Add_Block (State);
      Add_Block (State);
   end Restart_Game;

   function Has_2048 (Board : t_Board) return Boolean is
     (for some Row of Board => (for some Cell of Row => Cell = 2048));

   function Has_No_Move (State : t_Board_State) return Boolean is
     (State.Blanks = 0
        and then
        (for all Row in t_Board_Size =>
           (for all Column in 1 .. t_Board_Size'Last - 1 =>
              (State.Board(Row)(Column) /= State.Board(Row)(Column+1))))
        and then
        (for all Row in 1 .. t_Board_Size'Last - 1 =>
           (for all Column in t_Board_Size =>
              (State.Board(Row)(Column) /= State.Board(Row+1)(Column)))));

   procedure Update_Status (State : in out t_Board_State) is
   begin
      State.Game_Status :=
        (if Has_2048 (State.Board) then Won
         elsif Has_No_Move (State) then Lost
         else Playing);
   end Update_Status;


   procedure Add_Block (State : in out t_Board_State) is
      Block_Offset : Positive := Random_Int (Generator, 1, State.Blanks);
   begin
      State.Blanks := State.Blanks - 1;
      for Row of State.Board loop
         for Cell of Row loop
            if Cell = 0 then
               if Block_Offset = 1 then
                  Cell := (if Random_Int (Generator, 1, 10) = 1 then 4 else 2);
                  return;
               else
                  Block_Offset := Block_Offset - 1;
               end if;
            end if;
         end loop;
      end loop;

      if Has_No_Move (State) then
         State.Game_Status := Lost;
      end if;

   end Add_Block;

   procedure Move
     (Direction : t_Direction;
      State     : t_Board_State;
      New_State : out t_Board_State) is

      -- Moving and merging will always be performed leftward, hence
      -- the following transforms
      function Flip_Row (What : in t_Row) return t_Row is
         i : Natural := 0;
      begin
         return Result : t_Row do
            for Cell of reverse Result loop
               i := i + 1;
               Cell := What (i);
            end loop;
         end return;
      end Flip_Row;

      function Flip_Board (What : in t_Board) return t_Board is
         i : Natural := 0;
      begin
         return Result : t_Board do
            for Row of Result loop
               i := i + 1;
               Row := Flip_Row (What (i));
            end loop;
         end return;
      end Flip_Board;

      function Transpose (What : in t_Board) return t_Board is
      begin
         return Result : t_Board do
            for Row in t_Board'Range loop
               for Column in t_Row'Range loop
                  Result(Column)(Row) := What(Row)(Column);
               end loop;
            end loop;
         end return;
      end Transpose;

      -- For moving/merging, recursive expression functions will be
      -- used, but they can't contain statements, hence the following
      -- sub-function used by Merge
      function Add_Blank (Delta_Score : in Natural) return t_List is
      begin
         New_State.Blanks := New_State.Blanks+1;
         New_State.Score  := New_State.Score + Delta_Score;
         return (1 => 0);
      end Add_Blank;

      function Move_Row (What : in t_List) return t_List is
        (if What'Length = 1 then What
         elsif What(What'First) = 0
         then Move_Row(What(What'First+1..What'Last)) & (1 => 0)
         else (1 => What(What'First)) & Move_Row(What(What'First+1..What'Last)));

      function Merge (What : in t_List) return t_List is
        (if What'Length <= 1 or else What(What'First) = 0 then What
         elsif What(What'First) = What(What'First+1)
         then (1 => 2*What(What'First)) & Merge(What(What'First+2..What'Last))
            & Add_Blank(What(What'First))
         else (1 => What(What'First)) & Merge(What(What'First+1..What'Last)));

      function Move (What : in t_Board) return t_Board is
      begin
         return Result : t_Board do
            for Row in t_Board'Range loop
               Result (Row) := Merge (Move_Row (What (Row)));
            end loop;
         end return;
      end Move;

   begin
      New_State := State;

      case Direction is
         when Left  =>
            New_State.Board := Move (State.Board);
         when Right =>
            New_State.Board := Flip_Board (Move (Flip_Board (State.Board)));
         when Up    =>
            New_State.Board := Transpose (Move (Transpose (State.Board)));
         when Down  =>
            New_State.Board := Transpose (Flip_Board (Move (Flip_Board (Transpose (State.Board)))));
      end case;

      Update_Status (New_State);

   end Move;


begin
   System.Random_Numbers.Reset(Generator);
end Game;
