with Ada.Directories;
with Ada.Text_IO;
with Ada.Environment_Variables;

with Dotenv;

package body Storage is

   package Env renames Ada.Environment_Variables;

   Best_Score_Name : constant String := "PLAY_2048_BEST_SCORE";

   function Best_Score return Natural is
   begin
      if Env.Exists (Best_Score_Name) then
         return Natural'Value (Env.Value (Best_Score_Name));
      else
         return 0;
      end if;

   exception
      when Constraint_Error =>

         Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error,
                              "Error: invalid format for env var " &
                                Best_Score_Name);
         return 0;
   end Best_Score;

   procedure Save_State
     (Best_Score : Natural) is

      File : Ada.Text_IO.File_Type;
   begin

      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.Out_File, Dotenv.DEFAULT_FILENAME);
      exception
         when Ada.Text_IO.Name_Error =>
            Ada.Text_IO.Create
              (File, Ada.Text_IO.Out_File, Dotenv.DEFAULT_FILENAME);
      end;

      Ada.Text_IO.Put_Line (File, Best_Score_Name & "=" & Best_Score'Image);
      Ada.Text_IO.Close (File);

   end Save_State;

begin
   if Ada.Directories.Exists (Dotenv.DEFAULT_FILENAME) then
      Dotenv.Config;
   end if;
end Storage;
