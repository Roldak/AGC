with AGC;

procedure Test is
   Ex : exception;

   type Integer_Access is access Integer;

   procedure Foo is
      procedure Bar is
         type Local_Access is access all Integer;
         X : aliased Integer := 42;
         Y : Local_Access := X'Access;
      begin
         raise Ex;
      end Bar;

      V : Integer_Access := new Integer'(4);
   begin
      Bar;
   exception
      when Ex =>
         AGC.Collect;
   end Foo;
begin
   Foo;
end Test;
