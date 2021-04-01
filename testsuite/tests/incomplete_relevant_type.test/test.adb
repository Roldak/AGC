procedure Test is
   type T;
   type T_Access is access T;
   type T is record
      X : T_Access;
   end record;
begin
   null;
end Test;
