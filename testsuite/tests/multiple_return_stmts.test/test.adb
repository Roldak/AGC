with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   function Inner (B : Boolean; X : Integer) return Integer_Access is
   begin
      if B then
         return new Integer'(X + 1);
      else
         return new Integer'(X - 1);
      end if;
   end Inner;

   A : Integer := Inner (True, 42).all;
begin
   AGC.Collect;
end Test;
