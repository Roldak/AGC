with Ada.Text_IO; use Ada.Text_IO;
with Pkg;

procedure Main is
   X : Pkg.T_Access := Pkg.Create (3);
begin
   Put_Line (X.X'Image);
end Main;
