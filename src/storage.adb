with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with TOML.File_IO;

package body Storage is

   use TOML;

   Best_Score_Key : constant String := "best_score";
   Fullscreen_Mode_Key : constant String := "fullscreen_mode";
   Theme_Key : constant String := "theme_id";
   Filename : constant String := "play_2048.toml";

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



   function Best_Score return Natural is
      (Get_Natural (Key => Best_Score_Key, Default => 0));

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

   procedure Save_State
     (Best_Score : Natural;
      Fullscreen_Mode : Boolean;
      Theme           : Natural) is

      File : Ada.Text_IO.File_Type;
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

      Config.Set (Key => Fullscreen_Mode_Key,
                  Entry_Value => Create_Boolean (Fullscreen_Mode));

      Config.Set (Key => Theme_Key,
                  Entry_Value => Create_Integer (Any_Integer (Theme)));

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
                             "Error: while loading " & Filename & ": ");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Ada.Strings.Unbounded.To_String (Result.Message));
         end if;
      end;

   end if;

end Storage;
