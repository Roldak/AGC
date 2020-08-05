package Pkg is
   type T is record
      X : Integer;
   end record;
   type T_Access is access all T;

   function Create (X : Integer) return T_Access;
end Pkg;
