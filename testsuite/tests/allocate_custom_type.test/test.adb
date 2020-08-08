procedure Test is
   type T is record
      X : Integer;
   end record;

   type T_Access is access T;

   X : T_Access := new T'(X => 2);
begin
   AGC.Collect;
end Test;
