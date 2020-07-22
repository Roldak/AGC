with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   function Inner (X : Integer) return Integer_Access is
   begin
      return new Integer'(X);
   end Inner;

   function Add (A, B, C : Integer) return Integer is
   begin
      return Inner (A).all + Inner (B).all + Inner (C).all;
   end Add;

   A : Integer := Add (1, 2, 3);
begin
   GC.Collect;
end Test;
