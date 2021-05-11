procedure Test is
   type Integer_Access is access Integer;

   X : Integer_Access := new Integer'(3);
   Y : Integer := X.all;
begin
   null;
end Test;
