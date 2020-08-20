with Ada.Numerics.Discrete_Random;

package body Grids is
   package Random_X_Positions is new Ada.Numerics.Discrete_Random (X_Range);
   package Random_Y_Positions is new Ada.Numerics.Discrete_Random (Y_Range);
   package Random_Directions  is new Ada.Numerics.Discrete_Random (Direction);

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
      return Random_Directions.Random (Random_Direction_Generator);
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

   function Get
     (G : in Grid; X, Y : Natural) return Item_Vectors.Vector
   is
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
   Random_Directions.Reset (Random_Direction_Generator,   154);
end Grids;
