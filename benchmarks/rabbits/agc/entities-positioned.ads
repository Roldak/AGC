with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
package Entities.Positioned is
   type Positioned is abstract new Entity with private;

   procedure AGC_Visit_Positioned_Private (X : System.Address) with
      Inline;
   procedure AGC_Visit_Positioned_Private_Classwide (X : System.Address) with
      Inline;
   procedure Initialize (E : in out Positioned; X, Y : Natural);

   function X (P : Positioned) return Natural;
   function Y (P : Positioned) return Natural;

   procedure Relocate
     (P : aliased in out Positioned; W : in out World; New_X, New_Y : Natural);
private

   type Positioned is abstract new Entity with record
      P_X, P_Y : Natural;
   end record;
   procedure AGC_Visit_Positioned (X : System.Address) with
      Inline;
   overriding procedure AGC_Visit (X : access Positioned) with
      Inline;
   procedure AGC_Visit_Positioned_Classwide (X : System.Address) with
      Inline;
end Entities.Positioned;
