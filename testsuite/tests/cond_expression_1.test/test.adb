procedure Test is
   type Integer_Access is access Integer;

   X : Integer_Access;
begin
   X :=
     (case True is
         when False => raise Program_Error,
         when True => new Integer'(42));
end Test;
