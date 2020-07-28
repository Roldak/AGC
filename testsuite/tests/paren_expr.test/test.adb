procedure Test is
   type Integer_Access is access all Integer;

   X : Integer_Access := (new Integer'(3));
begin
   null;
end Test;
