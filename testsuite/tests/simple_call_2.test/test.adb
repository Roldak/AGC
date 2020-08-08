with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   procedure Inner (X : Integer_Access) is
      Y : Integer_Access := new Integer'(2);
   begin
      null;
   end Inner;
begin
   Inner (new Integer'(1));
   AGC.Collect;
end Test;
