package Pkg is
   type T is new Integer;
   type T_Access is access T;

   function Create (X : Integer) return T_Access;
end Pkg;
