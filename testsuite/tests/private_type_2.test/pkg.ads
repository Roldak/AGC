package Pkg is
   type T is private;

   function Create (X : Integer) return T;

   function Get (X : T) return Integer;

private
   type Integer_Access is access Integer;
   type T is tagged record
      X : Integer_Access;
   end record;
end Pkg;
