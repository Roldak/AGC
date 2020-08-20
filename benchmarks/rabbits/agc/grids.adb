with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Discrete_Random;
package body Grids is
   procedure AGC_Visit_Grid_Private (X : System.Address) renames
     Grids.AGC_Visit_Grid;
   procedure AGC_Visit_Grid_Private_Classwide (X : System.Address) renames
     Grids.AGC_Visit_Grid_Classwide;
   procedure AGC_Visit_Cell (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type Rec_Access is access all Cell;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Cell renames Conv (X).all;
   begin
      declare
         C : aliased Grids.Item_Vectors.Vector := R.Items;
      begin
         Grids.AGC_Item_Vectors_Visitors.AGC_Visit_Vector_Private (C'Address);
      end;
   end AGC_Visit_Cell;
   procedure AGC_Visit_Grid (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type Rec_Access is access all Grid'Class;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Grid'Class renames Conv (X).all;
   begin
      declare
         C : aliased Grids.Matrix := R.Cells;
      begin
         Grids.AGC_Visit_Matrix (C'Address);
      end;
   end AGC_Visit_Grid;
   procedure AGC_Visit (X : access Grid) is
   begin
      AGC_Visit_Grid (X.all'Address);
   end AGC_Visit;
   procedure AGC_Visit_Grid_Classwide (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type T_Access is access all Grid'Class;
      for T_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion (System.Address, T_Access);
   begin
      Conv (X).AGC_Visit;
   end AGC_Visit_Grid_Classwide;
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   package Random_X_Positions is new Ada.Numerics.Discrete_Random (X_Range);
   package Random_Y_Positions is new Ada.Numerics.Discrete_Random (Y_Range);
   package Random_Directions is new Ada.Numerics.Discrete_Random (Direction);
   Random_X_Position_Generator : Random_X_Positions.Generator;
   Random_Y_Position_Generator : Random_Y_Positions.Generator;
   Random_Direction_Generator  : Random_Directions.Generator;
   procedure Random_Position (X, Y : out Natural) is
   begin
      X := Random_X_Positions.Random (Random_X_Position_Generator);
      Y := Random_Y_Positions.Random (Random_Y_Position_Generator);
   end Random_Position;
   function Random_Direction return Direction is
   begin
      return
        AGC_Ret : Direction :=
          Random_Directions.Random (Random_Direction_Generator) do
         null;
      end return;
   end Random_Direction;
   function Moved (X, Y : in out Natural; Dir : Direction) return Boolean is
   begin
      case Dir is
         when North =>
            if Y = 0 then
               return False;
            else
               Y := Y - 1;
            end if;
         when South =>
            if Y = Height - 1 then
               return False;
            else
               Y := Y + 1;
            end if;
         when West =>
            if X = 0 then
               return False;
            else
               X := X - 1;
            end if;
         when East =>
            if X = Width - 1 then
               return False;
            else
               X := X + 1;
            end if;
      end case;
      return True;
   end Moved;
   procedure Put (G : in out Grid; X, Y : Natural; I : Item) is
   begin
      G.Cells (X, Y).Items.Append (I);
      G.Count := G.Count + 1;
   end Put;
   procedure Del (G : in out Grid; X, Y : Natural; I : Item) is
      Items : Item_Vectors.Vector renames G.Cells (X, Y).Items;
   begin
      for J in Items.First_Index .. Items.Last_Index loop
         if Items (J) = I then
            Items.Delete (J);
            G.Count := G.Count - 1;
            return;
         end if;
      end loop;
      raise Program_Error with "Element not found";
   end Del;
   function Get (G : in Grid; X, Y : Natural) return Item_Vectors.Vector is
   begin
      return G.Cells (X, Y).Items;
   end Get;
   function Item_Count (G : Grid) return Natural is
   begin
      return G.Count;
   end Item_Count;
begin
   Random_X_Positions.Reset (Random_X_Position_Generator, 154);
   Random_Y_Positions.Reset (Random_Y_Position_Generator, 154);
   Random_Directions.Reset (Random_Direction_Generator, 154);
end Grids;
