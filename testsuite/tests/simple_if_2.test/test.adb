with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Int_Access is access Integer;

   X : Int_Access := new Integer'(1);
   Y : Integer;
begin
   if True then
      Put_Line (X.all'Image);
   end if;
   Y := 2;
end Test;
