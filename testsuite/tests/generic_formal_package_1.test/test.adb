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
      with package P is new Pkg (<>);
   package G is
      procedure Foo;
   end G;

   package body G is
      procedure Foo is
         X : P.T := P.Get;
      begin
         null;
      end Foo;
   end G;

   package Pkg_I is new Pkg (Integer);
   package G_I is new G (Pkg_I);
begin
   G_I.Foo;
end Test;
