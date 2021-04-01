procedure Test is
   type Integer_Access is access Integer;

   type T is record
      X     : Integer;
      Dummy : Integer_Access := null;
   end record;

   type T_Access is access T;

   type U is record
      A : T_Access;
   end record;

   type U_Access is access U;

   X : U_Access := new U'(A => new T'(X => 1, others => <>));
begin
   AGC.Collect;
   X.A.X := 2;
end Test;
