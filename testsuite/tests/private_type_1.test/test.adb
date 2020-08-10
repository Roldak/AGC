with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   package P is
      type T is private;

      function Create (X : Integer) return T;
      function Get (X : T) return Integer;
   private
      type T is access all Integer;
   end P;

   package body P is
      function Create (X : Integer) return T is
      begin
         return new Integer'(X);
      end Create;

      function Get (X : T) return Integer is
      begin
         return X.all;
      end Get;
   end P;

   X : P.T := P.Create (2);
begin
   AGC.Collect;
   Put_Line (P.Get (X)'Image);
end Test;
