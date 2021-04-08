with AGC;
with Ada.Containers.Vectors;

procedure Test is
   package Foo is
      type T is abstract tagged null record;

      type T_Access is access T'Class;
   end Foo;

   package Bar is
      package Int_Vectors is new Ada.Containers.Vectors
        (Positive, Integer);

      type U is new Foo.T with record
         V : Int_Vectors.Vector;
      end record;
   end Bar;

   package Baz is
      type U is new Foo.T with record
         V : Integer := 2;
      end record;
   end Baz;

   procedure Main is
      X : Foo.T_Access := new Bar.U;
   begin
      null;
   end Main;
begin
   Main;
   AGC.Collect;
end Test;
