procedure Test is
   type Integer_Access is access all Integer
      with Size => Standard'Address_Size;
begin
   null;
end Test;
