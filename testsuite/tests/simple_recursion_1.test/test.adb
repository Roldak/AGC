with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   procedure Foo (N : Natural) is
      X : Integer_Access := new Integer'(42);
   begin
      if N < 100 then
         Foo (N + 1);
      end if;
   end Foo;
begin
   Foo (0);
   GC.Collect;
end Test;
