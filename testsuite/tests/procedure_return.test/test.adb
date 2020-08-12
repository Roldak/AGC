procedure Test is
   type Integer_Access is access all Integer;

   function Id (X : Integer_Access) return Integer_Access is
   begin
      return X;
   end Id;

   procedure Foo is
      X : Integer_Access := new Integer'(2);
   begin
      if X.all = 2 then
         X := Id (new Integer'(42));
         return;
      end if;

      X.all := 3;
   end Foo;
begin
   Foo;
   AGC.Collect;
end Test;
