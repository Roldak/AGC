with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   procedure Inner (X, Y : Integer_Access) is
   begin
      null;
   end Inner;
begin
   Inner (new Integer'(1), new Integer'(2));
   GC.Collect;
end Test;
