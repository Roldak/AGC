with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image; use System;

procedure Test is
   type Integer_Access is access all Integer;

   type Int_Arr is array (Positive range <>) of Integer_Access;

   X : Int_Arr :=
     (1 => new Integer'(42),
      2 => new Integer'(12),
      3 => new Integer'(21));
begin
   GC.Collect;
   X (2).all := 145;
end Test;
