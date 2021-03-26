with AGC;
procedure Test is
   package Pkg_Itf is
      type I is interface;
      type I_Access is access I'Class;
   end Pkg_Itf;

   type Integer_Access is access Integer;

   package Pkg_T1 is
      type T is new Pkg_Itf.I with record
         X : Integer_Access;
      end record;
   end Pkg_T1;
begin
   declare
      X : Pkg_Itf.I_Access;
   begin
      X := new Pkg_T1.T'(X => new Integer'(42));
      AGC.Collect;
      Pkg_T1.T (X.all).X.all := 43;
   end;
   AGC.Collect;
end Test;
