with Ada.Containers.Vectors;

generic
   Width  : Natural;
   Height : Natural;
   type Item is private;
package Grids is
   subtype X_Range is Natural range 0 .. Width - 1;
   subtype Y_Range is Natural range 0 .. Height - 1;

   type Direction is (North, South, East, West);

   procedure Random_Position (X, Y : out Natural);

   function Random_Direction return Direction;

   function Moved (X, Y : in out Natural; Dir : Direction) return Boolean;

   package Item_Vectors is new Ada.Containers.Vectors
     (Positive, Item);

   type Grid is tagged private;
   procedure Put (G : in out Grid; X, Y : Natural; I : Item);
   procedure Del (G : in out Grid; X, Y : Natural; I : Item);
   function  Get
     (G : in Grid; X, Y : Natural) return Item_Vectors.Vector;

   function Item_Count (G : Grid) return Natural;
private
   type Cell is record
      Items : Item_Vectors.Vector;
   end record;

   type Matrix is array (X_Range, Y_Range) of Cell;

   type Grid is tagged record
      Cells : Matrix;
      Count : Natural := 0;
   end record;
end Grids;
