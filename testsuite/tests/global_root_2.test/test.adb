procedure Test is
   type Integer_Access is access Integer;

   package Pkg is
      X : Integer_Access;
   end Pkg;
begin
   Pkg.X := new Integer'(2);
   AGC.Collect;
   Pkg.X := new Integer'(Pkg.X.all + 1);
end Test;
