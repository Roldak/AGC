procedure Test is
   type T is record
      X : Integer;
   end record;

   type T_Access is access T;

   type U is record
      A : T_Access;
   end record;

   type U_Access is access U;

   X : U_Access := new U'(A => new T'(X => 1));
begin
   AGC.Collect;
   X.A.X := 2;
end Test;
