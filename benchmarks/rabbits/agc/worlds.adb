with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Entities;
with Entities.Positioned;
with Entities.Rabbits;
with Entities.Grass;
with Entities.Wolves;
package body Worlds is
   procedure AGC_Visit_Entity_Access_Implem is new AGC.Visit_Access_Type
     (Entities.Entity'Class, Entity_Access, False,
      Entities.AGC_Visit_Entity_Private_Classwide);
   procedure AGC_Visit_Entity_Access (X : System.Address) renames
     AGC_Visit_Entity_Access_Implem;
   procedure AGC_Visit_Positioned_Access_Implem is new AGC.Visit_Access_Type
     (Entities.Positioned.Positioned'Class, Positioned_Access, True,
      Entities.Positioned.AGC_Visit_Positioned_Private_Classwide);
   procedure AGC_Visit_Positioned_Access (X : System.Address) renames
     AGC_Visit_Positioned_Access_Implem;
   procedure AGC_Visit_World_Private (X : System.Address) renames
     Worlds.AGC_Visit_World;
   procedure AGC_Visit_World_Private_Classwide (X : System.Address) renames
     Worlds.AGC_Visit_World_Classwide;
   procedure AGC_Visit_World (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type Rec_Access is access all World'Class;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : World'Class renames Conv (X).all;
   begin
      declare
         C : aliased Worlds.Entity_Vectors.Vector := R.Entities;
      begin
         Worlds.AGC_Entity_Vectors_Visitors.AGC_Visit_Vector_Private
           (C'Address);
      end;
      declare
         C : aliased Worlds.Entity_Vectors.Vector := R.New_Entities;
      begin
         Worlds.AGC_Entity_Vectors_Visitors.AGC_Visit_Vector_Private
           (C'Address);
      end;
      declare
         C : aliased Worlds.Grid.Grid := R.Cells;
      begin
         Worlds.Grid.AGC_Visit_Grid_Private (C'Address);
      end;
   end AGC_Visit_World;
   procedure AGC_Visit (X : access World) is
   begin
      AGC_Visit_World (X.all'Address);
   end AGC_Visit;
   procedure AGC_Visit_World_Classwide (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type T_Access is access all World'Class;
      for T_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion (System.Address, T_Access);
   begin
      Conv (X).AGC_Visit;
   end AGC_Visit_World_Classwide;
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   use type Ada.Containers.Count_Type;
   procedure Spawn (W : in out World; E : Entity_Access) is
   begin
      W.New_Entities.Append (E);
   end Spawn;
   procedure Update (W : in out World) is
   begin
      for I in reverse W.Entities.First_Index .. W.Entities.Last_Index loop
         declare
            E : constant Entity_Access := W.Entities (I);
         begin
            E.all.Update (W);
            if not E.all.Is_Alive then
               W.Entities.Delete (I);
               if E.all in Entities.Positioned.Positioned'Class then
                  declare
                     P : constant Positioned_Access := Positioned_Access (E);
                  begin
                     W.Cells.Del (P.X, P.Y, P);
                  end;
               end if;
            end if;
         end;
      end loop;
      for E of W.New_Entities loop
         E.all.Start (W);
         W.Entities.Append (E);
         if E.all in Entities.Positioned.Positioned'Class then
            declare
               P : constant Positioned_Access := Positioned_Access (E);
            begin
               W.Cells.Put (P.X, P.Y, P);
            end;
         end if;
      end loop;
      W.New_Entities.Clear;
   end Update;
   procedure Move
     (W : in out World; I : Positioned_Access; Old_X, Old_Y : Natural)
   is
   begin
      W.Cells.Del (Old_X, Old_Y, I);
      W.Cells.Put (I.X, I.Y, I);
   end Move;
   function Is_Running (W : in out World) return Boolean is
   begin
      return
        AGC_Ret : Boolean :=
          not W.Entities.Is_Empty or else not W.New_Entities.Is_Empty do
         null;
      end return;
   end Is_Running;
   function Located (W : in World; X, Y : Natural) return Positioned_Array is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
      Acc : aliased constant Grid.Item_Vectors.Vector := W.Cells.Get (X, Y);
   begin
      AGC.Push_Root
        (Acc'Address,
         Worlds.Grid.AGC_Item_Vectors_Visitors.AGC_Visit_Vector_Private'
           Address);
      return Res : Positioned_Array (Acc.First_Index .. Acc.Last_Index) do
         for I in Res'Range loop
            Res (I) := Acc (I);
         end loop;
         AGC.Pop_Roots (AGC_Base_Root_Count);
      end return;
   end Located;
   procedure Stats (W : World; Rabbits, Wolves, Grass : out Natural) is
      R, F, G : Natural := 0;
   begin
      for E of W.Entities loop
         if E.all in Entities.Rabbits.Rabbit'Class then
            R := R + 1;
         elsif E.all in Entities.Wolves.Wolf'Class then
            F := F + 1;
         elsif E.all in Entities.Grass.Grass'Class then
            G := G + 1;
         end if;
      end loop;
      Rabbits := R;
      Wolves  := F;
      Grass   := G;
   end Stats;
   function Has_Grass (W : World; X, Y : Natural) return Boolean is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
   begin
      declare
         AGC_Root_Count : constant Natural := AGC.Root_Count;
         AGC_Temp_0     : aliased Worlds.Grid.Item_Vectors.Vector :=
           W.Cells.Get (X, Y);
      begin
         AGC.Push_Root
           (AGC_Temp_0'Address,
            Worlds.Grid.AGC_Item_Vectors_Visitors.AGC_Visit_Vector_Private'
              Address);
         for E of AGC_Temp_0 loop
            if E.all in Entities.Grass.Grass'Class then
               AGC.Pop_Roots (AGC_Base_Root_Count);
               return True;
            end if;
         end loop;
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      AGC.Pop_Roots (AGC_Base_Root_Count);
      return False;
   end Has_Grass;
end Worlds;
