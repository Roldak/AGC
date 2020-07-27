procedure Test is
   type T is record
      X : Integer;
   end record;

   type T_Access is access all T;

   type U is record
      A : T_Access;
   end record;

   type U_Access is access all U;

   X : U_Access := new U'(A => new T'(X => 1));
begin
   GC.Collect;
   X.A.X := 2;
end Test;
