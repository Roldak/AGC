with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System; use System;

with AGC;

procedure Test is
   type My_Integer_Access is access Integer
      with Storage_Pool => AGC.Non_Managed'Storage_Pool;

   function Alloc (X : Integer) return Address is
      R : My_Integer_Access := new Integer'(X);
   begin
      return R.all'Address;
   end Alloc;

   function Deref (X : Address) return Integer is
      function Convert is new Ada.Unchecked_Conversion
        (Address, My_Integer_Access);
   begin
      return Convert (X).all;
   end Deref;

   X : System.Address := Alloc (3);
begin
   AGC.Collect;
   Put_Line (Deref (X)'Image);
end Test;
