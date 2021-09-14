procedure Test is
   type T is record
      X : Integer;
   end record;

   type T_Access is access T;

   function Make_T return T_Access is (new T'(X => 12));

   function Ident (X : T_Access) return T_Access is (X);

   X : constant T_Access := T_Access (Make_T);
   Y : constant T_Access := Ident (Make_T);
begin
   null;
end Test;
