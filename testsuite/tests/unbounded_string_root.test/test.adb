with Ada.Strings.Unbounded;

procedure Test is
   use Ada.Strings.Unbounded;

   X : Unbounded_String;
begin
   Append (X, "hello");
end Test;
