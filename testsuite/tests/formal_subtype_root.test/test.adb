procedure Test is
   generic
      type T is private;
   package Pkg is
      subtype U is T;

      procedure Foo;
   end Pkg;

   package body Pkg is
      procedure Foo is
         X : U;
      begin
         null;
      end Foo;
   end Pkg;
begin
   null;
end Test;
