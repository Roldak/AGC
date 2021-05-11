procedure Test is
   type Integer_Access is access Integer;

   function Foo (X : Integer) return Integer is
      Y : Integer_Access := new Integer'(X);
   begin
      return V : Integer := 0 do
         V := Y.all;
      end return;
   end Foo;
begin
   null;
end Test;
