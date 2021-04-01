with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System; use System;

with AGC;

procedure Test is
   type Integer_Access is access Integer;

   type Rec is record
      X : Integer_Access;
   end record;

   type My_Rec_Access is access Rec
      with Storage_Pool => AGC.Non_Managed'Storage_Pool;

   function Alloc (X : Integer) return My_Rec_Access is
      (new Rec'(X => new Integer'(X)));

   X : My_Rec_Access := Alloc (3);
begin
   AGC.Collect;
   Put_Line (X.X.all'Image);
end Test;
