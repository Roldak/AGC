procedure Test is
   type Integer_Access is access Integer;

   procedure Inner (X : Integer_Access) is
      Y : Integer_Access := new Integer'(2);
   begin
      Y := X;
   end Inner;

   A : Integer_Access := new Integer'(1);
begin
   Inner (A);
end Test;
