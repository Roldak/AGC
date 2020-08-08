with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   procedure Inner (X, Y : Integer_Access) is
   begin
      null;
   end Inner;
begin
   Inner (new Integer'(1), new Integer'(2));
   AGC.Collect;
end Test;
