procedure Test is
   type Integer_Access is access all Integer;

   --  unconstrained array
   type Int_Arr is array (Positive range <>) of Integer_Access;

   --  constrained array
   type Char_Arr is array (Character) of Integer_Access;

   X : Int_Arr :=
     (1 => new Integer'(42),
      2 => new Integer'(12),
      3 => new Integer'(21));

   Y : Char_Arr;
begin
   GC.Collect;
   X (2).all := 145;
   Y ('b') := X (3);
   GC.Collect;
end Test;
