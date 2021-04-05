with Ada.Text_IO; use Ada.Text_IO;
with AGC.Storage.Get;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System; use System;

package body AGC.Storage.Controlled is
   overriding procedure Collect
     (Self : in out Controlled_Pool; X : System.Address)
   is null;

   overriding function Is_Valid_Address
     (Self : in out Controlled_Pool; X : System.Address) return Boolean
   is
   begin
      return Get.AGC_Pool.Is_Valid_Address (X);
   end Is_Valid_Address;

   overriding procedure Allocate
     (Self : in out Controlled_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
   begin
      Get.AGC_Pool.Allocate (Addr, Size, Alignment);
   end Allocate;

   overriding procedure Deallocate
     (Self : in out Controlled_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
   begin
      Get.AGC_Pool.Deallocate (Addr, Size, Alignment);
   end Deallocate;
end AGC.Storage.Controlled;
