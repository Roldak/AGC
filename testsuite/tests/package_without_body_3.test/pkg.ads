package Pkg is
   type Integer_Access is access Integer;

   type Rec1 is record
      X : Integer_Access;
   end record;

   package Inner is
      type Rec2 is record
         X : Rec1;
      end record;

      type Rec3 is record
         X : Rec2;
      end record;
   end Inner;

   type Rec4 is record
      X : Inner.Rec3;
   end record;
end Pkg;
