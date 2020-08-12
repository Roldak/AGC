procedure Test is
   type Integer_Access is access Integer;

   function F return Integer_Access is
   begin
      return new Integer'(15);
   end F;

   X : Integer_Access := (new Integer'(3));

   Y : Integer := Integer_Access (F).all;
begin
   null;
end Test;
