with Ada.Text_IO; use Ada.Text_IO;
with AGC;

with Pkg;

procedure Main is
   type Integer_Access is access all Integer;

   V : aliased Integer := 2;

   type Rec is record
      X : Integer_Access := V'Access;
   end record;

   package Recs is new Pkg (Rec);
begin
   declare
      X : Recs.T_Access := Recs.Create;
   begin
      AGC.Collect;
      Put_Line (X.all.X.all'Image);
   end;
   AGC.Collect;
end Main;
