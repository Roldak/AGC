with Ada.Text_IO; use Ada.Text_IO;
with AGC;

procedure Test is
   type Int_Access is access Integer;

   type Int_Access_Array is array (Positive range <>) of Int_Access;

   X : Int_Access_Array (1 .. 2) := (new Integer'(1), new Integer'(2));
begin
   AGC.Collect;
   Put_Line (X (1).all'Image);
   Put_Line (X (2).all'Image);
end Test;
