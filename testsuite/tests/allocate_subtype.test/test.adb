procedure Test is
   subtype My_Natural is Integer range 0 .. Integer'Last;
   type My_Nat_Access is access all My_Natural;

   X : My_Nat_Access := new My_Natural'(2);
begin
   null;
end Test;
