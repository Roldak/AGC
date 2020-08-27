procedure Test is
   generic
      type T is private;
      type U is private;
   package Pkg is
      function Foo (X : U) return T;
   end Pkg;

   package body Pkg is
      function Foo (X : U) return T is
         Y : T;
      begin
         return Y;
      end Foo;
   end Pkg;

   package My_Pkg is new Pkg (Integer, Boolean);
begin
   null;
end Test;
