with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Worlds; use Worlds;
package Entities is
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   type Entity is abstract tagged private;
   procedure AGC_Visit_Entity_Private (X : System.Address);
   procedure AGC_Visit_Entity_Private_Classwide (X : System.Address);
   procedure Start (E : in out Entity; W : in out World) is abstract;
   procedure Update (E : in out Entity; W : in out World) is abstract;
   procedure Delete (E : in out Entity);
   function Is_Alive (E : in Entity) return Boolean;
private
   type Entity is abstract tagged record
      Alive : Boolean := True;
   end record;
   procedure AGC_Visit_Entity (X : System.Address);
   procedure AGC_Visit (X : access Entity);
   procedure AGC_Visit_Entity_Classwide (X : System.Address);
end Entities;