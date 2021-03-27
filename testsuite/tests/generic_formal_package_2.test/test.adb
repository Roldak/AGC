procedure Test is
   generic
      type T is private;
   package Pkg is
      function Get return T;
   end Pkg;

   package body Pkg is
      function Get return T is
         R : T;
      begin
         return R;
      end Get;
   end Pkg;

   generic
      type U is private;
      with package P is new Pkg (U);
   package G is
      procedure Foo;
   end G;

   package body G is
      procedure Foo is
         X : U := P.Get;
      begin
         null;
      end Foo;
   end G;

   package Pkg_I is new Pkg (Integer);
   package G_I is new G (Integer, Pkg_I);
begin
   G_I.Foo;
end Test;
