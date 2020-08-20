with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Entities.Rabbits;
package body Entities.Wolves is
   procedure AGC_Visit_Wolf_Private (X : System.Address) renames
     Entities.Wolves.AGC_Visit_Wolf;
   procedure AGC_Visit_Wolf_Private_Classwide (X : System.Address) renames
     Entities.Wolves.AGC_Visit_Wolf_Classwide;
   procedure AGC_Visit_Wolf (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type Rec_Access is access all Wolf'Class;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Wolf'Class renames Conv (X).all;
   begin
      Entities.Positioned.AGC_Visit_Positioned_Private (X);
   end AGC_Visit_Wolf;
   overriding procedure AGC_Visit (X : access Wolf) is
   begin
      AGC_Visit_Wolf (X.all'Address);
   end AGC_Visit;
   procedure AGC_Visit_Wolf_Classwide (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type T_Access is access all Wolf'Class;
      for T_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion (System.Address, T_Access);
   begin
      Conv (X).AGC_Visit;
   end AGC_Visit_Wolf_Classwide;
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   package Bool_Generators is new Ada.Numerics.Discrete_Random (Boolean);
   Bool_Generator : Bool_Generators.Generator;
   function Create return Entity_Access is
      X, Y : Natural;
   begin
      Worlds.Grid.Random_Position (X, Y);
      return AGC_Ret : Entity_Access := Create (X, Y) do
         null;
      end return;
   end Create;
   function Create (X, Y : Natural) return Entity_Access is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
   begin
      declare
         AGC_Temp_0 : aliased Worlds.Entity_Access := new Wolf;
      begin
         AGC.Push_Root
           (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
         return R : Entity_Access := AGC_Temp_0 do
            Positioned_Access (R).Initialize (X, Y);
            AGC.Pop_Roots (AGC_Base_Root_Count);
         end return;
      end;
   end Create;
   overriding procedure Start (R : in out Wolf; W : in out World) is
   begin
      R.Age  := 0;
      R.Food := 100;
   end Start;
   procedure Try_Reproducing (Self, Other : in out Wolf; W : in out World) is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
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
      declare
         AGC_Temp_0 : aliased Worlds.Entity_Access := Create (Self.X, Self.Y);
      begin
         AGC.Push_Root
           (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
         W.Spawn (AGC_Temp_0);
      end;
      AGC.Pop_Roots (AGC_Base_Root_Count);
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
   overriding procedure Update (R : in out Wolf; W : in out World) is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
      X                   : Natural          := R.X;
   begin
      declare
         Y : Natural := R.Y;
      begin
         declare
            Already_Reproduced : Boolean := False;
         begin
            if R.Age > 50 or R.Food < 2 then
               R.Delete;
               return;
            end if;
            R.Age  := R.Age + 1;
            R.Food := R.Food - 2;
            while not Worlds.Grid.Moved (X, Y, Worlds.Grid.Random_Direction)
            loop
               null;
            end loop;
            declare
               AGC_Root_Count : constant Natural := AGC.Root_Count;
               AGC_Temp_0     : aliased Worlds.Positioned_Array :=
                 W.Located (X, Y);
            begin
               AGC.Push_Root
                 (AGC_Temp_0'Address,
                  Worlds.AGC_Visit_Positioned_Array'Address);
               for E of AGC_Temp_0 loop
                  if E.all in Wolf'Class and not Already_Reproduced then
                     Try_Reproducing (R, Wolf (E.all), W);
                     Already_Reproduced := True;
                  elsif E.all in Rabbits.Rabbit'Class then
                     Try_Eat (R, Rabbits.Rabbit (E.all), W);
                  end if;
               end loop;
               AGC.Pop_Roots (AGC_Root_Count);
            end;
            R.Relocate (W, X, Y);
         end;
      end;
      AGC.Pop_Roots (AGC_Base_Root_Count);
   end Update;
end Entities.Wolves;
