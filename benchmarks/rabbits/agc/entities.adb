with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
package body Entities is
   procedure AGC_Visit_Entity_Private (X : System.Address) renames
     Entities.AGC_Visit_Entity;
   procedure AGC_Visit_Entity_Private_Classwide (X : System.Address) renames
     Entities.AGC_Visit_Entity_Classwide;
   procedure AGC_Visit_Entity (X : System.Address) is
      pragma Suppress (All_Checks);
      type Rec_Access is access Entity'Class with
        Storage_Size => 0;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Entity'Class renames Conv (X).all;
   begin
      null;
   end AGC_Visit_Entity;
   procedure AGC_Visit (X : access Entity) is
   begin
      AGC_Visit_Entity (X.all'Address);
   end AGC_Visit;
   procedure AGC_Visit_Entity_Classwide (X : System.Address) is
      pragma Suppress (All_Checks);
      type T_Access is access Entity'Class with
        Storage_Size => 0;
      for T_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion (System.Address, T_Access);
   begin
      Conv (X).AGC_Visit;
   end AGC_Visit_Entity_Classwide;
   function Is_Alive (E : in Entity) return Boolean is
   begin
      return (E.Alive);
   end Is_Alive;
   procedure Delete (E : in out Entity) is
   begin
      E.Alive := False;
   end Delete;
end Entities;
