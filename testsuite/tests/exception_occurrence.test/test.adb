with Ada.Exceptions;

procedure Test is
   procedure Foo (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      null;
   end Foo;
begin
   raise Program_Error;
exception
   when E : Program_Error =>
      Foo (E);
end Test;
