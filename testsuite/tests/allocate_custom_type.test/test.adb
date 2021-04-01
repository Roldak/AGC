procedure Test is
   type Integer_Access is access Integer;

   type T is record
      X : Integer_Access;
   end record;

   type T_Access is access T;

   X : T_Access := new T'(X => null);
begin
   AGC.Collect;
end Test;
