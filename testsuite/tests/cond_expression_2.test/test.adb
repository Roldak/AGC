procedure Test is
   type Integer_Access is access Integer;

   function Foo (X : Integer_Access) return Integer_Access is (X);

   X : Integer :=
     (case True is
         when False => raise Program_Error,
         when True => Foo (new Integer'(42)).all);
begin
   null;
end Test;
