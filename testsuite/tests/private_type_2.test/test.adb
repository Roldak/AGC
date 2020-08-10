with Ada.Text_IO; use Ada.Text_IO;
with Pkg;

procedure Test is
   X : Pkg.T := Pkg.Create (2);
begin
   AGC.Collect;
   Put_Line (Pkg.Get (X)'Image);
end Test;
