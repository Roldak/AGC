with AGC;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   X : Integer_Access := null;
begin
   for I in 1 .. 10_000 loop
      X := new Integer'(42);
   end loop;
   AGC.Collect;
end Test;
