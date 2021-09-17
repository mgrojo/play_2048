with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Calendar;

with TOML.File_IO;

package body Storage is

   use TOML;
   use type Ada.Calendar.Time;

   Best_Score_Key : constant String := "best_score";
   Fullscreen_Mode_Key : constant String := "fullscreen_mode";
   Theme_Key : constant String := "theme_id";
   Board_State_Key : constant String := "board_state";
   Score_Key : constant String := "score";
   Elapsed_Key : constant String := "elapsed";
   Best_Elapsed_Key : constant String := "best_elapsed";

   -- In Unix-like OS, save the configuration file as dot-file in the
   -- home as usual.  In others, save in current directory.
   --
   Filename : constant String :=
     (if Ada.Environment_Variables.Exists ("HOME") then
         Ada.Environment_Variables.Value ("HOME") & "/." else
         "") & "play_2048.toml";

   Config : TOML_Value;


   function Get_Natural
     (Key     : String;
      Default : Natural) return Natural is
   begin
      if Config /= No_TOML_Value and then
        Config.Has (Key) then

         return Natural (Config.Get (Key).As_Integer);
      else
         return Default;
      end if;

   exception
      when Constraint_Error =>

         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Error: invalid format for " & Key);

         return Default;
   end Get_Natural;


   function Get_Float
     (Key     : String;
      Default : Valid_Float) return Valid_Float is
   begin
      if Config /= No_TOML_Value and then
        Config.Has (Key) then

         return Config.Get (Key).As_Float.Value;
      else
         return Default;
      end if;

   exception
      when Constraint_Error =>

         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Error: invalid format for " & Key);

         return Default;
   end Get_Float;


   function Best_Score return Natural is
      (Get_Natural (Key => Best_Score_Key, Default => 0));

   -- Some versions of GNAT do not allow Duration values in
   -- Ada.Calendar.Formatting greater or equal to 24h, so we can only
   -- set here a maximum of 24h - 1s.  GNAT 10.3.0 works right and
   -- supports up to 99h as required by the Standard.  23:59:59 should
   -- be good as an initial Best Time to beat.
   --
   function Best_Time return Duration is
     (Duration (Get_Float (Key => Best_Elapsed_Key,
                           Default => Valid_Float
                             (Ada.Calendar.Day_Duration'Last - 1.0))));

   function Theme return Natural is
      (Get_Natural (Key => Theme_Key, Default => 1));


   function Fullscreen_Mode return Boolean is
      Default : constant Boolean := False;
   begin
      if Config /= No_TOML_Value and then
        Config.Has (Key => Fullscreen_Mode_Key) then

         return Config.Get (Fullscreen_Mode_Key).As_Boolean;
      else
         return Default;
      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                              "Error: invalid format for " &
                                Fullscreen_Mode_Key);
         return Default;
   end Fullscreen_Mode;

   procedure Restore_Game
     (Game_State : out Game.t_Board_State) is

      Board_State_Value : TOML_Value;

      Index : Natural := 1;
   begin

      if Config /= No_TOML_Value and then
        Config.Has (Key => Board_State_Key) then

         Game.Reset_Game (Game_State);

         Game_State.Score := Get_Natural (Key => Score_Key, Default => 0);

         Game_State.Start_Time := Ada.Calendar.Clock -
           Duration (Get_Float (Key => Elapsed_Key, Default => 0.0));

         Board_State_Value := Config.Get (Board_State_Key);

         if Board_State_Value.Length /= Game.t_Cell_Count'Last then

            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item => "Warning: Board size has changed, " &
                 Board_State_Key &
                 " is not restored");

            Game.Restart_Game (Game_State);
         else

            for i in Game_State.Board'range loop
               for j in Game_State.Board(i)'range loop

                  Game_State.Board (i)(j) :=
                    Natural (Board_State_Value.Item
                               (Index).As_Integer);

                  Index := Index + 1;

                  if Game_State.Board (i)(j) /= 0 then
                     Game_State.Blanks := Game_State.Blanks - 1;
                  end if;

               end loop;
            end loop;

         end if;

         Game.Update_Status (Game_State);

      else

         Game.Restart_Game (Game_State);
      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Error: invalid format for " &
                                 Board_State_Key);

         Game.Restart_Game (Game_State);

   end Restore_Game;

   procedure Save_State
     (Best_Score      : Natural;
      Best_Time       : Duration;
      Fullscreen_Mode : Boolean;
      Theme           : Natural;
      Game_State      : Game.t_Board_State) is

      File : Ada.Text_IO.File_Type;
      Board_State_Value : TOML_Value;
   begin

      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.Out_File, Filename);
      exception
         when Ada.Text_IO.Name_Error =>
            Ada.Text_IO.Create
              (File, Ada.Text_IO.Out_File, Filename);
      end;

      if Config = No_TOML_Value then
         Config := Create_Table;
      end if;

      Config.Set (Key => Best_Score_Key,
                  Entry_Value => Create_Integer (Any_Integer (Best_Score)));

      Config.Set (Key         => Best_Elapsed_Key,
                  Entry_Value => Create_Float
                    ((Kind  => Regular,
                      Value => Valid_Float (Best_Time))));


      Config.Set (Key => Fullscreen_Mode_Key,
                  Entry_Value => Create_Boolean (Fullscreen_Mode));

      Config.Set (Key => Theme_Key,
                  Entry_Value => Create_Integer (Any_Integer (Theme)));

      Board_State_Value := Create_Array (Item_Kind => TOML_Array);

      for i in Game_State.Board'range loop
         for j in Game_State.Board(i)'range loop

            Board_State_Value.Append
              (Item => Create_Integer (Any_Integer (Game_State.Board(i)(j))));
         end loop;
      end loop;

      Config.Set (Key => Board_State_Key,
                  Entry_Value => Board_State_Value);

      Config.Set (Key => Score_Key,
                  Entry_Value => Create_Integer (Any_Integer (Game_State.Score)));

      Config.Set (Key => Elapsed_Key,
                  Entry_Value => Create_Float
                    ((Kind => Regular,
                      Value => Valid_Float
                        (Ada.Calendar."-" (Ada.Calendar.Clock, Game_State.Start_Time)))));

      TOML.File_IO.Dump_To_File (Config, File);

      Ada.Text_IO.Close (File);

   end Save_State;

begin
   if Ada.Directories.Exists (Filename) then

      declare
         Result : constant TOML.Read_Result :=
           TOML.File_IO.Load_File (Filename);
      begin
         if Result.Success then
            Config := Result.Value;
         else
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             "Error: while loading " & Filename & ":" &
                               Result.Location.Line'Image & ":" &
                               Result.Location.Column'Image & ": ");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Ada.Strings.Unbounded.To_String (Result.Message));
         end if;
      end;

   end if;

end Storage;
