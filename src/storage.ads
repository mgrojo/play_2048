with Game;

package Storage is

   function Best_Score return Natural;

   function Best_Time return Duration;

   function Fullscreen_Mode return Boolean;

   function Theme return Natural;

   procedure Restore_Game
     (Game_State : out Game.t_Board_State);

   procedure Save_State
     (Best_Score      : Natural;
      Best_Time       : Duration;
      Fullscreen_Mode : Boolean;
      Theme           : Natural;
      Game_State      : Game.t_Board_State);

end Storage;
