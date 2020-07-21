with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   X : Integer_Access := null;
   Y : Integer := 12;
begin
   null;
end Test;
