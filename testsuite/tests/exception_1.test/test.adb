procedure Test is
   Error : exception;
   type Integer_Access is access Integer;
begin
   declare
   begin
      declare
         X : Integer_Access := new Integer'(2);
      begin
         raise Error;
      end;
   exception
      when Error =>
         null;
   end;
   AGC.Collect;
end Test;
