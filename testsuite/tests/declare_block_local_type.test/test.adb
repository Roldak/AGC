procedure Test is
begin
   Foo : declare
      type T is access Integer;

      X : T := new Integer'(2);
   begin
      X.all := 3;
   end Foo;
end Test;
