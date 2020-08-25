package Pkg is
   procedure Foo;

   package Inner is
      type Rec is record
         X : Integer;
      end record;
   end Inner;
end Pkg;
