with Ada.Text_IO;
use Ada.Text_IO;

procedure Test is
   type Int_Access is access Integer;

   type Int_Array is array (Positive range <>) of Int_Access;

   X : Int_Array (1 .. 10) := (others => new Integer'(1));
   Y : Int_Array (1 .. 2)  := (1 | 2 => new Integer'(1));
begin
   X (1).all := 2;
   Y (1).all := 2;

   Put_Line (X (2).all'Image);
   Put_Line (Y (2).all'Image);
end Test;
