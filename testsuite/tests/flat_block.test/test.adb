procedure Test is
   type Integer_Access is access Integer;

   X : Integer_Access := new Integer'(1);
   Y : Integer_Access := new Integer'(2);
   Z : Integer_Access := new Integer'(3);
begin
   declare
      A : Integer_Access := new Integer'(4);
      B : Integer_Access := new Integer'(5);
      C : Integer_Access := new Integer'(6);
   begin
      null;
   end;
end Test;
