with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Test is
   type Integer_Access is access all Integer;

   package Int_Vectors is new Ada.Containers.Vectors
     (Positive, Integer_Access);

   V : Int_Vectors.Vector;
begin
   V.Append (new Integer'(1));
   V.Append (new Integer'(2));
   V.Append (new Integer'(3));
   AGC.Collect;
   for X of V loop
      Put_Line (X.all'Image);
   end loop;
end Test;
