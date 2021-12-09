with AGC;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   X : Integer_Access := new Integer'(0);
begin
   while X.all < 10000 loop
      X := new Integer'(X.all + 1);
   end loop;
end Test;
