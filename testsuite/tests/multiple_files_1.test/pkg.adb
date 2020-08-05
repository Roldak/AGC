package body Pkg is
   function Create (X : Integer) return T_Access is
   begin
      return new T'(T (X));
   end Create;
end Pkg;
