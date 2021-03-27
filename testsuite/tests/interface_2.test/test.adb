procedure Test is
   package Pkg is
      type I is interface;

      function Foo (X : I) return I is abstract;
      function Get (X : I) return Integer is abstract;
   end Pkg;

   procedure Bar (X : Pkg.I'Class) is
      Y : Integer := X.Foo.Get;
   begin
      null;
   end Bar;
begin
   null;
end Test;
