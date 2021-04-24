with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;
   type Int_Array is array (Positive range <>) of Integer_Access;

   X : Int_Array (2 .. 10) := (others => new Integer'(1));
begin
   X (3).all := 2;
   Put_Line (X (4).all'Image);
   null;
end Test;
