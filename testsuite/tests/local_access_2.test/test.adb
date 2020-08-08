with System.Address_Image;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   type Arr is array (1 .. 2) of aliased Integer;

   R : aliased Arr := (others => 0);
   A : aliased Integer_Access  := R (2)'Unchecked_Access;
begin
   AGC.Collect;
   Put_Line (R (1)'Image);
end Test;
