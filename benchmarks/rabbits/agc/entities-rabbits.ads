with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Entities.Positioned;

package Entities.Rabbits is
   type Rabbit is new Positioned.Positioned with private;

   procedure AGC_Visit_Rabbit_Private (X : System.Address) with
     Inline;
   procedure AGC_Visit_Rabbit_Private_Classwide (X : System.Address) with
     Inline;
   function Create return Entity_Access;
   function Create (X, Y : Natural) return Entity_Access;

   overriding procedure Start (R : in out Rabbit; W : in out World);
   overriding procedure Update (R : in out Rabbit; W : in out World);
private
   type Rabbit is new Positioned.Positioned with record
      Age  : Natural;
      Food : Natural;
   end record;
   procedure AGC_Visit_Rabbit (X : System.Address) with
     Inline;
   overriding procedure AGC_Visit (X : access Rabbit) with
     Inline;
   procedure AGC_Visit_Rabbit_Classwide (X : System.Address) with
     Inline;
end Entities.Rabbits;
