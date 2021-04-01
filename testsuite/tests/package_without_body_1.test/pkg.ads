package Pkg is
   type Integer_Access is access Integer;

   type Rec is record
      X : Integer_Access;
   end record;
end Pkg;
