with Ada.Calendar;
with Ada.Environment_Variables;

package Game is

   type t_Direction is (Up, Down, Right, Left);

   Environment_Size : constant String := "PLAY_2048_SIZE";

   Default_Length : constant := 4;

   Board_Length : constant Natural :=
     (if Ada.Environment_Variables.Exists (Environment_Size) then
         Natural'Value (Ada.Environment_Variables.Value (Environment_Size))
      else Default_Length);

   subtype t_Board_Size is Natural range 1 .. Board_Length;
   subtype t_Cell_Count is Natural range 0 .. Board_Length ** 2;

   type    t_List  is array (Positive range <>) of Natural;
   subtype t_Row   is t_List (t_Board_Size);
   type    t_Board is array  (t_Board_Size) of t_Row;

   type t_Board_State is record
      Board : t_Board;
      Score : Natural;
      Blanks : t_Cell_Count;
      Start_Time : Ada.Calendar.Time;
   end record;

   function Elapsed_Time (State : t_Board_State) return Duration;

   procedure Reset_Game (State : out t_Board_State);
   procedure Restart_Game (State : out t_Board_State);

   procedure Add_Block (State : in out t_Board_State);

   procedure Move
     (Direction : t_Direction;
      State     : t_Board_State;
      New_State : out t_Board_State);

   function Has_Won (Board : t_Board) return Boolean is
     (for some Row of Board => (for some Cell of Row => Cell = 2048));

   function Has_Lost (State : t_Board_State) return Boolean is
     (State.Blanks = 0
        and then (for all Row in t_Board_Size =>
                    (for all Column in 1 .. t_Board_Size'Last - 1 =>
                       (State.Board(Row)(Column) /= State.Board(Row)(Column+1))))
        and then (for all Row in 1 .. t_Board_Size'Last - 1 =>
                    (for all Column in t_Board_Size =>
                       (State.Board(Row)(Column) /= State.Board(Row+1)(Column)))));

end Game;
