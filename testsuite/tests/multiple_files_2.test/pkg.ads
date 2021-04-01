package Pkg is
   type Integer_Access is access Integer;

   type T is record
      X     : Integer;
      Dummy : Integer_Access := null;
   end record;
   type T_Access is access T;

   function Create (X : Integer) return T_Access;
end Pkg;
