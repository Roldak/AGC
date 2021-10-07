procedure Test is
   package Pkg is
      type T is private;
      type U is private;
      type A is array (Integer range <>) of T;

   private
      type T is record
         Value : U;
      end record;
      type U is access Integer;
   end Pkg;

   X : Pkg.A (1 .. 10);
begin
   null;
end Test;
