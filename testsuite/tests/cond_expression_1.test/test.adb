with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   X : Integer_Access;
begin
   X :=
     (case True is
         when False => raise Program_Error,
         when True => new Integer'(42));
   Put_Line (X.all'Image);
end Test;
