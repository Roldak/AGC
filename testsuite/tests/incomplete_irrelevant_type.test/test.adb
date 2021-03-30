procedure Test is
   type Int;
   type Int_Access is access Int;
   type Int is range 1 .. 10;

   X : Int_Access := null;
begin
   null;
end Test;
