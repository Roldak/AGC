with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Containers.Vectors;

generic
   Width : Natural;
   Height : Natural;
   type Item is private;
   with procedure AGC_Visit_Item_Private (X : System.Address);
package Grids is
   subtype X_Range is Natural range 0 .. Width - 1;
   subtype Y_Range is Natural range 0 .. Height - 1;

   type Direction is (North, South, East, West);

   procedure Random_Position (X, Y : out Natural);

   function Random_Direction return Direction;

   function Moved (X, Y : in out Natural; Dir : Direction) return Boolean;

   package Item_Vectors is new Ada.Containers.Vectors (Positive, Item);

   package AGC_Item_Vectors_Visitors is new AGC.Standard
     .Ada_Containers_Vectors_Visitors
     (Item_Vectors, AGC_Visit_Index_Type => AGC.No_Op,
      AGC_Visit_Element_Type             => Grids.AGC_Visit_Item_Private);
   type Grid is tagged private;
   procedure AGC_Visit_Grid_Private (X : System.Address) with
      Inline;
   procedure AGC_Visit_Grid_Private_Classwide (X : System.Address) with
      Inline;
   procedure Put (G : in out Grid; X, Y : Natural; I : Item);
   procedure Del (G : in out Grid; X, Y : Natural; I : Item);
   function Get (G : in Grid; X, Y : Natural) return Item_Vectors.Vector;

   function Item_Count (G : Grid) return Natural;
private
   type Cell is record
      Items : Item_Vectors.Vector;
   end record;

   procedure AGC_Visit_Cell (X : System.Address) with
      Inline;
   type Matrix is array (X_Range, Y_Range) of Cell;

   procedure AGC_Visit_Matrix is new AGC.Visit_Constrained_Array_2_Type
     (Cell, X_Range, Y_Range, Matrix, Grids.AGC_Visit_Cell);
   type Grid is tagged record
      Cells : Matrix;
      Count : Natural := 0;
   end record;
   procedure AGC_Visit_Grid (X : System.Address) with
      Inline;
   procedure AGC_Visit (X : access Grid) with
      Inline;
   procedure AGC_Visit_Grid_Classwide (X : System.Address) with
      Inline;
end Grids;
