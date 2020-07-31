with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   procedure Foo (N : Integer_Access) is
      X : Integer_Access := new Integer'(N.all + 1);
   begin
      if X.all < 100 then
         Foo (X);
      end if;
   end Foo;
begin
   Foo (new Integer'(0));
   AGC.Collect;
end Test;
