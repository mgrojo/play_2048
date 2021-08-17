
-- This isn't portable and raises a warning. It should be replaced by
-- the standard way in Ada 202x.
-- http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0144-1.txt?rev=1.13
with System.Random_Numbers;

package body Game is

   function Random_Int is new System.Random_Numbers.Random_Discrete(Integer);
   Generator : System.Random_Numbers.Generator;

   procedure Reset_Game (State : out t_Board_State) is
   begin
      State.Board  := (others => (others => 0));
      State.Blanks := t_Cell_Count'Last;
      State.Score  := 0;
      Add_Block (State);
      Add_Block (State);
   end Reset_Game;

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
   end Add_Block;

   procedure Move
     (Direction : t_Direction;
      State     : t_Board_State;
      New_State : out t_Board_State) is

      -- Moving and merging will always be performed leftward, hence the following transforms
      function HFlip (What : in t_Row) return t_Row is
        (What(4), What(3), What(2), What(1));
      function VFlip (What : in t_Board) return t_Board is
        (HFlip(What(1)), HFlip(What(2)), HFlip(What(3)), HFlip(What(4)));
      function Transpose (What : in t_Board) return t_Board is
      begin
         return Answer : t_Board do
            for Row in t_Board'Range loop
               for Column in t_Row'Range loop
                  Answer(Column)(Row) := What(Row)(Column);
               end loop;
            end loop;
         end return;
      end Transpose;

      -- For moving/merging, recursive expression functions will be used, but they
      -- can't contain statements, hence the following sub-function used by Merge
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
         then (1 => 2*What(What'First)) & Merge(What(What'First+2..What'Last)) & Add_Blank(What(What'First))
         else (1 => What(What'First)) & Merge(What(What'First+1..What'Last)));

      function Move (What : in t_Board) return t_Board is
        (Merge(Move_Row(What(1))),
         Merge(Move_Row(What(2))),
         Merge(Move_Row(What(3))),
         Merge(Move_Row(What(4))));

   begin
      New_State := State;

      case Direction is
         when Left  =>
            New_State.Board := Move (State.Board);
         when Right =>
            New_State.Board := VFlip (Move (VFlip (State.Board)));
         when Up    =>
            New_State.Board := Transpose (Move (Transpose (State.Board)));
         when Down  =>
            New_State.Board := Transpose (VFlip (Move (VFlip (Transpose (State.Board)))));
      end case;


   end Move;


begin
   System.Random_Numbers.Reset(Generator);
end Game;
