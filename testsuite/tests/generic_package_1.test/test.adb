procedure Test is
   generic
      type T is private;
   package Pkg is
      function Foo return T;
   end Pkg;

   package body Pkg is
      function Foo return T is
         X : T;
      begin
         return X;
      end Foo;
   end Pkg;
begin
   null;
end Test;
