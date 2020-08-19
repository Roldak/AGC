procedure Test is
   type Integer_Access is access all Integer;

   X : Integer_Access := new Integer'(2);
   Y : Integer_Access := X;
   Z : constant Integer_Access := Y;
begin
   null;
end Test;
