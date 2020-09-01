procedure Test is
   type Integer_Access is access Integer;

   package Pkg is
      function Incr (X : Integer_Access) return Integer_Access is
        (new Integer'(X.all + 1));
   end Pkg;

   X : Integer_Access;
begin
   X := Pkg.Incr (new Integer'(2));
end Test;
