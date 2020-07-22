with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   function Inner (X : Integer) return Integer_Access is
      Tmp : Integer_Access := new Integer'(X);
   begin
      return Tmp;
   end Inner;

   function Add (A, B : Integer_Access) return Integer is
   begin
      return A.all + B.all;
   end Add;

   A : Integer := Add (Inner (1), Inner (2));
begin
   GC.Collect;
end Test;
