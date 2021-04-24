with Ada.Text_IO; use Ada.Text_IO;
with AGC;

procedure Test is
   type Int_Access is access Integer;

   function Foo return Int_Access is
   begin
      AGC.Collect;
      return new Integer'(1);
   end Foo;

   type Int_Array is array (Positive range <>) of Int_Access;

   X : Int_Array (1 .. 10) := (others => Foo);
   Y : Int_Array (1 .. 2)  := (1 | 2 => Foo);
begin
   Put_Line (X (3).all'Image);
   Put_Line (Y (2).all'Image);
end Test;
