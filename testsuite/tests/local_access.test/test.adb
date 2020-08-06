with System.Address_Image;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   type Rec is record
      X : aliased Integer;
      Y : aliased Integer;
   end record;

   R : aliased Rec := (others => 0);
   A : aliased Integer_Access  := R.Y'Unchecked_Access;
begin
   AGC.Collect;
   Put_Line (R.X'Image);
end Test;
