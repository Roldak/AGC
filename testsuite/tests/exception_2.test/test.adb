procedure Test is
   Error : exception;
   type Integer_Access is access Integer;

   procedure Foo is
   begin
      declare
         X : Integer_Access := new Integer'(2);
      begin
         raise Error;
      end;
   exception
      when Error =>
         null;
   end Foo;
begin
   Foo;
   AGC.Collect;
end Test;
