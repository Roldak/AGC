package body Pkg is
   function Create return T_Access is
   begin
      return new T;
   end Create;
end Pkg;
