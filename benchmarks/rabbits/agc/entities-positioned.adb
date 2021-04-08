with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
package body Entities.Positioned is
   procedure AGC_Visit_Positioned_Private (X : System.Address) renames
     Entities.Positioned.AGC_Visit_Positioned;
   procedure AGC_Visit_Positioned_Private_Classwide
     (X : System.Address) renames
     Entities.Positioned.AGC_Visit_Positioned_Classwide;
   procedure AGC_Visit_Positioned (X : System.Address) is
      pragma Suppress (All_Checks);
      type Rec_Access is access Positioned'Class with
         Storage_Size => 0;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Positioned'Class renames Conv (X).all;
   begin
      Entities.AGC_Visit_Entity (X);
   end AGC_Visit_Positioned;
   overriding procedure AGC_Visit (X : access Positioned) is
   begin
      AGC_Visit_Positioned (X.all'Address);
   end AGC_Visit;
   procedure AGC_Visit_Positioned_Classwide (X : System.Address) is
      pragma Suppress (All_Checks);
      type T_Access is access Positioned'Class with
         Storage_Size => 0;
      for T_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion (System.Address, T_Access);
   begin
      Conv (X).AGC_Visit;
   end AGC_Visit_Positioned_Classwide;
   procedure Initialize (E : in out Positioned; X, Y : Natural) is
   begin
      E.P_X := X;
      E.P_Y := Y;
   end Initialize;
   function X (P : Positioned) return Natural is
   begin
      return P.P_X;
   end X;
   function Y (P : Positioned) return Natural is
   begin
      return P.P_Y;
   end Y;
   procedure Relocate
     (P : aliased in out Positioned; W : in out World; New_X, New_Y : Natural)
   is
      Old_X : Natural := P.P_X;
      Old_Y : Natural := P.P_Y;
   begin
      P.P_X := New_X;
      P.P_Y := New_Y;
      W.Move (P'Unchecked_Access, Old_X, Old_Y);
   end Relocate;
end Entities.Positioned;
