with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   function Foo return Integer_Access is (null);

   X : Integer_Access;
begin
   X :=
     (if False then raise Program_Error
      elsif Foo /= null then raise Program_Error
      else new Integer'(42));
   Put_Line (X.all'Image);
end Test;
