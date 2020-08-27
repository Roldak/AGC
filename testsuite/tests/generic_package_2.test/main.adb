with Ada.Text_IO; use Ada.Text_IO;

with Pkg;

procedure Main is
   type Rec is record
      X : Integer := 2;
   end record;

   package Recs is new Pkg (Rec);
begin
   declare
      X : Recs.T_Access := Recs.Create;
   begin
      AGC.Collect;
      Put_Line (X.all.X'Image);
   end;
   AGC.Collect;
end Main;
