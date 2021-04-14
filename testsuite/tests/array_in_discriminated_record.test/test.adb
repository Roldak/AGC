with Ada.Text_IO; use Ada.Text_IO;
with AGC;

procedure Test is
   type Int_Access is access Integer;
   type Int_Access_Array is array (Positive range <>) of Int_Access;
   type T (N : Natural) is record
      X : Int_Access_Array (1 .. N);
   end record;

   X : T := (N => 2, X => (new Integer'(1), new Integer'(2)));
begin
   AGC.Collect;
   Put_Line (X.X (1).all'Image);
   Put_Line (X.X (2).all'Image);
end Test;
