with AGC;
with Ada.Finalization;

procedure Test is
   type Integer_Access is access Integer;

   package Foo is
      type T is private;
   private
      type T is new Ada.Finalization.Controlled with record
         X : Integer_Access;
      end record;
   end Foo;

   package Bar is
      type T_Access is access Foo.T;
   end Bar;

   procedure Main is
      X : Bar.T_Access := new Foo.T;
   begin
      null;
   end Main;
begin
   Main;
   AGC.Collect;
end Test;
