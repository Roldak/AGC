with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Entities.Positioned;
package Entities.Grass is
   type Grass is new Positioned.Positioned with private;
   procedure AGC_Visit_Grass_Private (X : System.Address);
   procedure AGC_Visit_Grass_Private_Classwide (X : System.Address);
   function Create return Entity_Access;
   function Create (X, Y : Natural) return Entity_Access;
   overriding procedure Start (G : in out Grass; W : in out World);
   overriding procedure Update (G : in out Grass; W : in out World);
   function Eat (G : in out Grass) return Boolean;
private
   type Grass is new Positioned.Positioned with record
      Content : Natural;
   end record;
   procedure AGC_Visit_Grass (X : System.Address);
   overriding procedure AGC_Visit (X : access Grass);
   procedure AGC_Visit_Grass_Classwide (X : System.Address);
end Entities.Grass;
