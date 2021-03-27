procedure Test is
   generic
      type T is private;
   package Pkg is
      type Rec is record
         X : T;
      end record;
   end Pkg;
begin
   null;
end Test;
