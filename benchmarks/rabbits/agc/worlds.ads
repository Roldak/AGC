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
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   type Entity_Access is access Entities.Entity'Class;
   procedure AGC_Visit_Entity_Access (X : System.Address);
   type Entity_Array is array (Positive range <>) of Entity_Access;
   procedure AGC_Visit_Entity_Array is new AGC.Visit_Unconstrained_Array_1_Type
     (Entity_Access, Positive, Entity_Array, Worlds.AGC_Visit_Entity_Access);
   type Positioned_Access is access all Entities.Positioned.Positioned'Class;
   procedure AGC_Visit_Positioned_Access (X : System.Address);
   type Positioned_Array is array (Positive range <>) of Positioned_Access;
   procedure AGC_Visit_Positioned_Array is new AGC
     .Visit_Unconstrained_Array_1_Type
     (Positioned_Access, Positive, Positioned_Array,
      Worlds.AGC_Visit_Positioned_Access);
   type World is tagged private;
   procedure AGC_Visit_World_Private (X : System.Address);
   procedure AGC_Visit_World_Private_Classwide (X : System.Address);
   package Grid is new Grids (50, 50, Positioned_Access);
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
     (Entity_Vectors, Worlds.AGC_Visit_Entity_Access);
   type World is tagged record
      Entities     : Entity_Vectors.Vector;
      New_Entities : Entity_Vectors.Vector;
      Cells        : Grid.Grid;
   end record;
   procedure AGC_Visit_World (X : System.Address);
   procedure AGC_Visit (X : access World);
   procedure AGC_Visit_World_Classwide (X : System.Address);
end Worlds;
