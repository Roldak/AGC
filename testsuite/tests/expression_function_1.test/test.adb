procedure Test is
   type Integer_Access is access Integer;

   function Incr (X : Integer_Access) return Integer_Access is
     (new Integer'(X.all + 1));
begin
   null;
end Test;
