package body Pkg is
   function Create (X : Integer) return T_Access is
   begin
      return new T'(X => X, others => null);
   end Create;
end Pkg;
