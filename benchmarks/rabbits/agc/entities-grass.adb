with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
package body Entities.Grass is
   procedure AGC_Visit_Grass_Private (X : System.Address) renames
     Entities.Grass.AGC_Visit_Grass;
   procedure AGC_Visit_Grass_Private_Classwide (X : System.Address) renames
     Entities.Grass.AGC_Visit_Grass_Classwide;
   procedure AGC_Visit_Grass (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type Rec_Access is access all Grass'Class;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Grass'Class renames Conv (X).all;
   begin
      Entities.Positioned.AGC_Visit_Positioned_Private (X);
   end AGC_Visit_Grass;
   overriding procedure AGC_Visit (X : access Grass) is
   begin
      AGC_Visit_Grass (X.all'Address);
   end AGC_Visit;
   procedure AGC_Visit_Grass_Classwide (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type T_Access is access all Grass'Class;
      for T_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion (System.Address, T_Access);
   begin
      Conv (X).AGC_Visit;
   end AGC_Visit_Grass_Classwide;
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
         AGC_Temp_0  : aliased Worlds.Entity_Access := new Grass;
         AGC_Dummy_0 : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         return G : Entity_Access := AGC_Temp_0 do
            Positioned_Access (G).Initialize (X, Y);
            AGC.Pop_Roots (AGC_Base_Root_Count);
         end return;
      end;
   end Create;
   overriding procedure Start (G : in out Grass; W : in out World) is
   begin
      G.Content := 5;
   end Start;
   overriding procedure Update (G : in out Grass; W : in out World) is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
      X                   : Natural          := G.X;
      Y                   : Natural          := G.Y;
   begin
      if Worlds.Grid.Moved (X, Y, Worlds.Grid.Random_Direction) then
         if not W.Has_Grass (X, Y) then
            declare
               AGC_Root_Count : constant Natural             := AGC.Root_Count;
               AGC_Temp_0     : aliased Worlds.Entity_Access := Create (X, Y);
               AGC_Dummy_0    : constant AGC.Empty_Type      :=
                 AGC.Push_Root
                   (AGC_Temp_0'Address,
                    Worlds.AGC_Visit_Entity_Access'Address);
            begin
               W.Spawn (AGC_Temp_0);
               AGC.Pop_Roots (AGC_Root_Count);
            end;
         end if;
      end if;
      AGC.Pop_Roots (AGC_Base_Root_Count);
   end Update;
   function Eat (G : in out Grass) return Boolean is
   begin
      if not G.Is_Alive then
         return False;
      end if;
      G.Content := G.Content - 1;
      if G.Content = 0 then
         G.Delete;
      end if;
      return True;
   end Eat;
end Entities.Grass;
