with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   package Pkg is
      type T is new Integer;
      type T_Access is access T;

      function Create (X : Integer) return T_Access;
   end Pkg;

   package body Pkg is
      function Create (X : Integer) return T_Access is
      begin
         return new T'(T (X));
      end Create;
   end Pkg;

   package Outer is
      package Inner is
         type Rec is record
            X : Pkg.T_Access;
         end record;

         procedure Foo (X : Rec);
      end Inner;
   end Outer;

   package body Outer is
      package body Inner is
         procedure Foo (X : Rec) is
            V : Pkg.T_Access := X.X;
         begin
            Put_Line (V.all'Image);
         end Foo;
      end Inner;
   end Outer;
begin
   Outer.Inner.Foo ((X => Pkg.Create (2)));
end Test;
