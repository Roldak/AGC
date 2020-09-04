procedure Test is
   type Integer_Access is access Integer;
begin
   declare
      package Pkg is
         X : Integer_Access;
      end Pkg;
   begin
      Pkg.X := new Integer'(2);
   end;
   AGC.Collect;
end Test;
