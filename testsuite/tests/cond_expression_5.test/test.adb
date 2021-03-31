procedure Test is
   type Integer_Access is access Integer;

   function Foo (X : Integer_Access) return Integer_Access is (X);
   function Bar (X : Integer_Access) return Integer is (X.all);

   X : Integer_Access := new Integer'
     (Bar (Foo
        (if False
         then raise Program_Error
         else Foo (Foo (new Integer'(42))))));
begin
   null;
end Test;
