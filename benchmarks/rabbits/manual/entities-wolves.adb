with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

with Entities.Rabbits;

package body Entities.Wolves is
   package Bool_Generators is new Ada.Numerics.Discrete_Random (Boolean);

   Bool_Generator : Bool_Generators.Generator;

   function Create return Entity_Access is
      X, Y : Natural;
   begin
      Worlds.Grid.Random_Position (X, Y);
      return Create (X, Y);
   end Create;

   function Create (X, Y : Natural) return Entity_Access is
   begin
      return R : Entity_Access := new Wolf do
         Positioned_Access (R).Initialize (X, Y);
      end return;
   end Create;

   overriding procedure Start
     (R : in out Wolf; W : in out World)
   is
   begin
      R.Age := 0;
      R.Food := 100;
   end Start;

   procedure Try_Reproducing
     (Self, Other : in out Wolf; W : in out World)
   is
   begin
      if Self.Age < 10 or Other.Age < 10 then
         return;
      end if;

      if Self.Food < 130 or Other.Food < 130 then
         return;
      end if;

      if Bool_Generators.Random (Bool_Generator) then
         return;
      end if;

      W.Spawn (Create (Self.X, Self.Y));
   end Try_Reproducing;

   procedure Try_Eat
     (Self : in out Wolf; Rabbit : in out Rabbits.Rabbit; W : in out World)
   is
   begin
      if Self.Food >= 190 then
         return;
      end if;

      if not Rabbit.Is_Alive then
         return;
      end if;

      Self.Food := Self.Food + 10;
      Rabbit.Delete;
   end Try_Eat;

   overriding procedure Update
     (R : in out Wolf; W : in out World)
   is
      X : Natural := R.X;
      Y : Natural := R.Y;
      Already_Reproduced : Boolean := False;
   begin
      if R.Age > 50 or R.Food < 2 then
         R.Delete;
         return;
      end if;

      R.Age := R.Age + 1;
      R.Food := R.Food - 2;

      while not Worlds.Grid.Moved
        (X, Y, Worlds.Grid.Random_Direction)
      loop
         null;
      end loop;

      for E of W.Located (X, Y) loop
         if E.all in Wolf'Class and not Already_Reproduced then
            Try_Reproducing (R, Wolf (E.all), W);
            Already_Reproduced := True;
         elsif E.all in Rabbits.Rabbit'Class then
            Try_Eat (R, Rabbits.Rabbit (E.all), W);
         end if;
      end loop;

      R.Relocate (W, X, Y);
   end Update;
end Entities.Wolves;
