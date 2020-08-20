with Ada.Containers.Vectors;
with Grids;

limited with Entities;
limited with Entities.Positioned;

package Worlds is
   type Entity_Access is access Entities.Entity'Class;
   type Entity_Array is array (Positive range <>) of Entity_Access;

   type Positioned_Access is access all Entities.Positioned.Positioned'Class;
   type Positioned_Array is array (Positive range <>) of Positioned_Access;

   type World is tagged private;

   package Grid is new Grids (50, 50, Positioned_Access);

   procedure Update (W : in out World);

   procedure Spawn (W : in out World; E : Entity_Access);

   procedure Move
     (W : in out World; I : Positioned_Access;
      Old_X, Old_Y : Natural);

   function Is_Running (W : in out World) return Boolean;

   function Located
     (W : in World; X, Y : Natural) return Positioned_Array;

   procedure Stats
     (W : World; Rabbits, Wolves, Grass : out Natural);

   function Has_Grass (W : World; X, Y : Natural) return Boolean;
private
   package Entity_Vectors is new Ada.Containers.Vectors
     (Positive, Entity_Access);

   type World is tagged record
      Entities     : Entity_Vectors.Vector;
      New_Entities : Entity_Vectors.Vector;
      Cells        : Grid.Grid;
   end record;
end Worlds;
