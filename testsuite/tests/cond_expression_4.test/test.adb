with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   function Foo return Integer_Access is (null);
   function Eq (X, Y : Integer_Access) return Integer_Access is
     (new Integer'(if X.all = Y.all then 1 else 0));
begin
   if Foo /= null and then Eq (Foo, new Integer'(3)).all = 1 then
      Put_Line ("hello");
   else
      Put_Line ("hi");
   end if;
end Test;
