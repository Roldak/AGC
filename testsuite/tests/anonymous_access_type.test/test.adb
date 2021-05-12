with AGC;

procedure Test is
begin
   declare
      X : access Integer := new Integer'(2);
      Y : access constant Integer := new Integer'(3);
   begin
      null;
   end;
   AGC.Collect;
end Test;
