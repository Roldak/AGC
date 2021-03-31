with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Entities.Positioned;
package Entities.Wolves is
   type Wolf is new Positioned.Positioned with private;
   procedure AGC_Visit_Wolf_Private (X : System.Address);
   procedure AGC_Visit_Wolf_Private_Classwide (X : System.Address);
   function Create return Entity_Access;
   function Create (X, Y : Natural) return Entity_Access;
   overriding procedure Start (R : in out Wolf; W : in out World);
   overriding procedure Update (R : in out Wolf; W : in out World);
private
   type Wolf is new Positioned.Positioned with record
      Age  : Natural;
      Food : Natural;
   end record;
   procedure AGC_Visit_Wolf (X : System.Address);
   overriding procedure AGC_Visit (X : access Wolf);
   procedure AGC_Visit_Wolf_Classwide (X : System.Address);
end Entities.Wolves;
