package Game is

   type t_Direction is (Up, Down, Right, Left);

   Board_Length : constant := 4;

   subtype t_Board_Size is Natural range 1 .. Board_Length;
   subtype t_Cell_Count is Natural range 0 .. Board_Length ** 2;

   type    t_List  is array (Positive range <>) of Natural;
   subtype t_Row   is t_List (t_Board_Size);
   type    t_Board is array  (t_Board_Size) of t_Row;

   type t_Board_State is record
      Board : t_Board;
      Score : Natural;
      Blanks : t_Cell_Count;
   end record;

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
