with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with TOML.File_IO;

package body Storage is

   use TOML;

   Best_Score_Name : constant String := "best_score";
   Fullscreen_Mode_Name : constant String := "fullscreen_mode";
   Filename : constant String := "play_2048.toml";

   Config : TOML_Value;

   function Best_Score return Natural is
   begin
      if Config /= No_TOML_Value and then
        Config.Has (Key => Best_Score_Name) then

         return Natural (Config.Get (Best_Score_Name).As_Integer);
      else
         return 0;
      end if;

   exception
      when Constraint_Error =>

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                              "Error: invalid format for " &
                                Best_Score_Name);
         return 0;
   end Best_Score;


   function Fullscreen_Mode return Boolean is
      Default : constant Boolean := False;
   begin
      if Config /= No_TOML_Value and then
        Config.Has (Key => Fullscreen_Mode_Name) then

         return Config.Get (Fullscreen_Mode_Name).As_Boolean;
      else
         return Default;
      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                              "Error: invalid format for " &
                                Fullscreen_Mode_Name);
         return Default;
   end Fullscreen_Mode;

   procedure Save_State
     (Best_Score : Natural;
      Fullscreen_Mode : Boolean) is

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

      Config.Set (Key => Best_Score_Name,
                  Entry_Value => Create_Integer (Any_Integer (Best_Score)));

      Config.Set (Key => Fullscreen_Mode_Name,
                  Entry_Value => Create_Boolean (Fullscreen_Mode));

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
