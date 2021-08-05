with Sf.Graphics.Texture;
with Sf.Graphics.Sprite;
with Sf.Graphics.RenderWindow;
with Sf.Graphics.Color;
with Sf.Graphics.Font;
with Sf.Graphics.Text;
with Sf.Graphics.Image;

with Sf.Window.VideoMode;
with Sf.Window.Event;
with Sf.Window.Keyboard;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Command_Line;

with System.Random_Numbers;

with Storage;

procedure Play_2048 is

   use Sf.Graphics;
   use Sf.Graphics.Color;
   use Sf.Window;
   use Sf;

   -- ----- Keyboard management
   type t_Keystroke is (Up, Down, Right, Left, Quit, Restart, Invalid);

   function Get_Keystroke (Key_Code : Keyboard.sfKeyCode) return t_Keystroke is
     (case Key_Code is
         when Keyboard.sfKeyQ => Quit,
         when Keyboard.sfKeyR => Restart,
         when Keyboard.sfKeyW | Keyboard.sfKeyLeft => Left,
         when Keyboard.sfKeyA | Keyboard.sfKeyUp => Up,
         when Keyboard.sfKeyS | Keyboard.sfKeyDown => Down,
         when Keyboard.sfKeyD | Keyboard.sfKeyRight => Right,
         when others => Invalid);

   -- ----- Game data
   function Random_Int is new System.Random_Numbers.Random_Discrete(Integer);
   type    t_List  is array (Positive range <>) of Natural;
   subtype t_Row   is t_List (1..4);
   type    t_Board is array  (1..4) of t_Row;

   Board      : t_Board;
   New_Board  : t_Board;
   Blanks     : Natural;
   Score      : Natural;
   Best       : Natural := 0;
   Generator  : System.Random_Numbers.Generator;
   Video_Mode : constant Window.VideoMode.sfVideoMode := (800, 600, 32);
   App_Win    : sfRenderWindow_Ptr := RenderWindow.create(Video_Mode, "2048 Game!");
   Icon       : sfImage_Ptr := Image.createFromFile("resources/icon.png");
   App_Event  : Event.sfEvent;
   Null_Event : Boolean := False;
   Tiles      : constant sfTexture_Ptr := Texture.createFromFile("resources/tiles.png");
   Tile_Size  : constant Positive := Positive (Texture.getSize (Tiles).y);
   Board_Size : constant Positive := Tile_Size * t_Board'Length;
   Margin     : constant Positive := (Positive (Video_Mode.Height) - Board_Size) / 2;
   Pane_Center : constant Positive := Positive (Video_Mode.Width) -
     (Positive (Video_Mode.Width) - Board_Size - Margin) / 2;
   Tile_Sprite : sfSprite_Ptr := Sprite.create;
   Game_Sprite : sfSprite_Ptr := Sprite.create;
   Game_Image : sfTexture_Ptr := Texture.createFromImage (Icon);
   Game_Font  : sfFont_Ptr := Font.createFromFile("resources/DejaVuSans-Bold.ttf");
   Score_Text : sfText_Ptr := Text.create;
   Game_Text  : sfText_Ptr := Text.create;
   Text_UI    : Boolean := False;
   Help       : constant String :=
     "R: restart game" & ASCII.LF &
     "Q: quit game" & ASCII.LF &
     "Arrows: move";


   procedure Display_Text (Message : String) is
      Board_Center : constant Float := Float (Margin + Board_Size / 2);

   begin
      if Text_UI then
         Put_Line (Message);
      end if;

      Text.setString (Game_Text, Message);
      Text.setPosition (Game_Text,
                        (x => Board_Center - Text.getLocalBounds (Game_Text).width / 2.0,
                         y => Board_Center - Text.getLocalBounds (Game_Text).height / 2.0));
      RenderWindow.drawText (App_Win, Game_Text);

   end Display_Text;

   -- ----- Displaying the board
   procedure Display_Board (Message : String := "") is
      Horizontal_Rule : constant String := "+----+----+----+----+";
      function Center (Value : in String) return String is
        ((1..(2-(Value'Length-1)/2) => ' ') & -- Add leading spaces
         Value(Value'First+1..Value'Last)   & -- Trim the leading space of the raw number image
         (1..(2-Value'Length/2) => ' '));     -- Add trailing spaces
   begin
      if Text_UI then
         Put_Line (Horizontal_Rule);
         for Row of Board loop
            for Cell of Row loop
               Put('|' & (if Cell = 0 then "    " else Center(Cell'Img)));
            end loop;
            Put_Line("|");
            Put_Line (Horizontal_Rule);
         end loop;
         Put_Line("Score =" & Score'Image & "  Best Score =" & Best'Image);
      end if;

      RenderWindow.clear(App_Win, sfWhite);

      for i in Board'range loop
         for j in Board(i)'range loop
            declare
               Cell : Natural renames Board(i)(j);
               Sprite_Offset : Natural := Natural (if Cell = 0 then 0.0
                                                   else Log (X    => Float (Cell),
                                                             Base => 2.0));
            begin
               Sprite.setTextureRect(Tile_Sprite, (Sprite_Offset * Tile_Size, 0, Tile_Size, Tile_Size));
               Sprite.setPosition(Tile_Sprite, (x => Float(Margin + (j - 1) * Tile_Size),
                                                y => Float(Margin + (i - 1) * Tile_Size)));
               RenderWindow.drawSprite(App_Win, Tile_Sprite);
            end;
         end loop;
      end loop;

      Text.setString (Score_Text, "Score" & ASCII.LF & Score'Image & ASCII.LF & ASCII.LF &
                        "Best score" & ASCII.LF & Best'Image);
      Text.setPosition (Score_Text,
                        (x => Float (Pane_Center) - Text.getLocalBounds (Score_Text).width / 2.0,
                         y => Float (Margin)));
      RenderWindow.drawText (App_Win, Score_Text);

      Sprite.setPosition(Game_Sprite,
                         (x => Float (Pane_Center -
                                        Positive (Image.getSize (Icon).x) / 2),
                          y => Float (Positive (Video_Mode.Height) - Margin -
                                        Positive (Image.getSize (Icon).y))));
      RenderWindow.drawSprite(App_Win, Game_Sprite);

      if Message /= "" then
         Display_Text(Message);
      end if;

      RenderWindow.display(App_Win);

   end Display_Board;

   procedure Set_Text_Style
     (The_Text : sfText_Ptr) is
   begin
      Text.setFont (The_Text, Game_Font);
      Text.setOutlineColor (The_Text, (R => 32, G => 32, B => 32, A => 255));
      Text.setFillColor (The_Text, (R => 196, G => 160, B => 0, A => 255));
      Text.setOutlineThickness (The_Text, 1.0);
   end Set_Text_Style;

   -- ----- Game mechanics
   procedure Add_Block is
      Block_Offset : Positive := Random_Int(Generator, 1, Blanks);
   begin
      Blanks := Blanks-1;
      for Row of Board loop
         for Cell of Row loop
            if Cell = 0 then
               if Block_Offset = 1 then
                  Cell := (if Random_Int(Generator,1,10) = 1 then 4 else 2);
                  return;
               else
                  Block_Offset := Block_Offset-1;
               end if;
            end if;
         end loop;
      end loop;
   end Add_Block;

   procedure Reset_Game is
   begin
      Board  := (others => (others => 0));
      Blanks := 16;
      Score  := 0;
      Add_Block;
      Add_Block;
   end Reset_Game;

   -- Moving and merging will always be performed leftward, hence the following transforms
   function HFlip (What : in t_Row) return t_Row is
     (What(4),What(3),What(2),What(1));
   function VFlip (What : in t_Board) return t_Board is
     (HFlip(What(1)),HFlip(What(2)),HFlip(What(3)),HFlip(What(4)));
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
      Blanks := Blanks+1;
      Score  := Score+Delta_Score;
      if Score > Best then
         Best := Score;
      end if;
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
     (Merge(Move_Row(What(1))),Merge(Move_Row(What(2))),Merge(Move_Row(What(3))),Merge(Move_Row(What(4))));

begin
   System.Random_Numbers.Reset(Generator);

   Text_UI := Ada.Command_Line.Argument_Count >= 1 and then
     Ada.Command_Line.Argument (1) = "-text";

   Sprite.setTexture (Tile_Sprite, Tiles);
   Sprite.setTexture (Game_Sprite, Game_Image);

   Set_Text_Style (Score_Text);
   Set_Text_Style (Game_Text);

   Text.setCharacterSize (Game_Text, Text.getCharacterSize (Game_Text) * 2);

   RenderWindow.setIcon
     (renderWindow => App_Win,
      width => Image.getSize (Icon).x,
      height => Image.getSize (Icon).y,
      pixels => Image.getPixelsPtr (Icon));

   Best := Storage.Best_Score;

   Main_Loop:
   loop

      Reset_Game;

      Game_Loop:
      loop

         if not Null_Event then
            Display_Board;
         end if;

         if RenderWindow.waitEvent (App_Win, event => App_Event) /= sfTrue then
            Null_Event := True;
         else
            Null_Event := False;
            case App_Event.eventType is
               when Event.sfEvtClosed =>

                  RenderWindow.Close (App_Win);
                  exit Main_Loop;

               when Event.sfEvtResized =>

                  Display_Board;
                  Null_Event := True;

               when Event.sfEvtKeyPressed =>

                  case Get_Keystroke (Key_Code => App_Event.key.code) is
                     when Restart => exit Game_Loop;
                     when Quit    => exit Main_Loop;
                     when Left    => New_Board := Move(Board);
                     when Right   => New_Board := VFlip(Move(VFlip(Board)));
                     when Up      => New_Board := Transpose(Move(Transpose(Board)));
                     when Down    => New_Board := Transpose(VFlip(Move(VFlip(Transpose(Board)))));
                     when others  =>
                        Display_Board (Message => Help);
                        Null_Event := True;
                  end case;

               when others => Null_Event := True;
            end case;

         end if;

         if Null_Event then
            null;
         elsif New_Board = Board then
            Display_Board (Message => "Invalid move...");
            Null_Event := True;
         elsif (for some Row of New_Board => (for some Cell of Row => Cell = 2048)) then
            Board := New_Board;
            Display_Board (Message => "You win!");
            Null_Event := True;
            exit Game_Loop;
         else
            Board := New_Board;
            Add_Block; -- OK since the board has changed
            if Blanks = 0
              and then (for all Row in 1..4 =>
                          (for all Column in 1..3 =>
                             (Board(Row)(Column) /= Board(Row)(Column+1))))
              and then (for all Row in 1..3 =>
                          (for all Column in 1..4 =>
                             (Board(Row)(Column) /= Board(Row+1)(Column)))) then
               Display_Board (Message => "You lose!");
               Null_Event := True;
               exit Game_Loop;
            end if;
         end if;
      end loop Game_Loop;
   end loop Main_Loop;

   Storage.Save_State
     (Best_Score => Best);

end Play_2048;
