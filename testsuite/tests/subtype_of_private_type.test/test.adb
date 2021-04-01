procedure Test is
   package Pkg is
      type T is private;
   private
      type Integer_Access is access Integer;
      type T is record
         X : Integer_Access;
      end record;
   end Pkg;

   subtype PT is Pkg.T;

   type R is record
      V : Natural;
      D : PT;
   end record;

   X : R;
begin
   null;
end Test;
