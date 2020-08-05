package body Pkg is
   function Create (X : Integer) return T_Access is
   begin
      return new T'(X => X);
   end Create;
end Pkg;
