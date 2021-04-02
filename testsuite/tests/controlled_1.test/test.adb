with AGC;
with Ada.Finalization;

procedure Test is
   package Pkg is
      type T is new Ada.Finalization.Controlled with record
         X : Integer;
      end record;

      type T_Access is access all T;
   end Pkg;

   use Pkg;

   procedure Main is
      X : T_Access := new T'
        (Ada.Finalization.Controlled with X => 3);
   begin
      null;
   end Main;
begin
   Main;
   AGC.Collect;
end Test;
