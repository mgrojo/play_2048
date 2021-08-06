package Storage is

   function Best_Score return Natural;

   function Fullscreen_Mode return Boolean;

   procedure Save_State
     (Best_Score : Natural;
      Fullscreen_Mode : Boolean);

end Storage;
