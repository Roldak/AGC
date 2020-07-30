procedure Test is
   type T is record
      X : Integer;
   end record;

   type T_Access is access all T;

   X : T_Access := new T'(X => 2);
begin
   GC.Collect;
end Test;
