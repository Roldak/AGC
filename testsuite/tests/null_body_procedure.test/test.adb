procedure Test is
   type Integer_Access is access Integer;

   package Pkg is
      type T is record
         X : Integer_Access;
      end record;

      procedure Foo (X : T) is null;
   end Pkg;

   procedure Bar (X : Integer_Access) is null;
begin
   null;
end Test;
