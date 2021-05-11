procedure Test is
   type Integer_Access is access Integer;
begin
   declare
      X : Integer_Access := new Integer'(2);
      Y : Integer;
   begin
      Y := X.all;
   end;
end Test;
