package Pkg is
   procedure Foo;

   package Inner is
      type Integer_Access is access Integer;

      type Rec is record
         X : Integer_Access;
      end record;
   end Inner;
end Pkg;
