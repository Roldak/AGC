package body Pkg is
   function Create (X : Integer) return T is
   begin
      return (X => new Integer'(X));
   end Create;

   function Get (X : T) return Integer is
   begin
      return X.X.all;
   end Get;
end Pkg;
