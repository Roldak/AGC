with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Containers.Vectors;
with Grids;
limited with Entities;
limited with Entities.Positioned;
package Worlds is
   type Entity_Access is access Entities.Entity'Class;
   for Entity_Access'Storage_Pool use AGC.Storage.Get.Pool;
   procedure AGC_Visit_Entity_Access (X : System.Address) with
      Inline;
   function AGC_Register_Entity_Access
     (X : Entity_Access) return Entity_Access with
      Inline;
   type Entity_Array is array (Positive range <>) of Entity_Access;
   procedure AGC_Visit_Entity_Array is new AGC.Visit_Unconstrained_Array_1_Type
     (Entity_Access, Positive, Entity_Array, Worlds.AGC_Visit_Entity_Access);
   type Positioned_Access is access all Entities.Positioned.Positioned'Class;
   for Positioned_Access'Storage_Pool use AGC.Storage.Get.Pool;
   procedure AGC_Visit_Positioned_Access (X : System.Address) with
      Inline;
   function AGC_Register_Positioned_Access
     (X : Positioned_Access) return Positioned_Access with
      Inline;
   type Positioned_Array is array (Positive range <>) of Positioned_Access;
   procedure AGC_Visit_Positioned_Array is new AGC
     .Visit_Unconstrained_Array_1_Type
     (Positioned_Access, Positive, Positioned_Array,
      Worlds.AGC_Visit_Positioned_Access);
   type World is tagged private;
   procedure AGC_Visit_World_Private (X : System.Address) with
      Inline;
   procedure AGC_Visit_World_Private_Classwide (X : System.Address) with
      Inline;
   package Grid is new Grids
     (50, 50, Positioned_Access,
      AGC_Visit_Item_Private => Worlds.AGC_Visit_Positioned_Access);
   procedure Update (W : in out World);
   procedure Spawn (W : in out World; E : Entity_Access);
   procedure Move
     (W : in out World; I : Positioned_Access; Old_X, Old_Y : Natural);
   function Is_Running (W : in out World) return Boolean;
   function Located (W : in World; X, Y : Natural) return Positioned_Array;
   procedure Stats (W : World; Rabbits, Wolves, Grass : out Natural);
   function Has_Grass (W : World; X, Y : Natural) return Boolean;
private
   package Entity_Vectors is new Ada.Containers.Vectors
     (Positive, Entity_Access);
   package AGC_Entity_Vectors_Visitors is new AGC.Standard
     .Ada_Containers_Vectors_Visitors
     (Entity_Vectors, AGC_Visit_Index_Type => AGC.No_Op,
      AGC_Visit_Element_Type               => Worlds.AGC_Visit_Entity_Access);
   type World is tagged record
      Entities     : Entity_Vectors.Vector;
      New_Entities : Entity_Vectors.Vector;
      Cells        : Grid.Grid;
   end record;
   procedure AGC_Visit_World (X : System.Address) with
      Inline;
   procedure AGC_Visit (X : access World) with
      Inline;
   procedure AGC_Visit_World_Classwide (X : System.Address) with
      Inline;
end Worlds;
