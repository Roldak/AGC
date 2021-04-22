procedure Test is
   Ex : exception;

   type Integer_Access is access Integer;

   procedure Foo is
      X : Integer_Access := new Integer'(2);
   begin
      raise Ex;
   exception
      when Ex =>
         declare
            Y : Integer_Access := new Integer'(3);
         begin
            null;
         end;
   end Foo;
begin
   Foo;
end Test;
