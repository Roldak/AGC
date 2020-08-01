with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;
with System.Memory; use System.Memory;

package body AGC.Storage.Bump_Ptr is
   Data_Size : constant Storage_Count := 1024 * 1024 * 1024;

   procedure Collect
     (Self : in out Bump_Ptr_Pool; X : System.Address)
   is
   begin
      null;
   end Collect;

   procedure Finalize (Self : in out Bump_Ptr_Pool) is
      use type System.Address;
   begin
      if Self.Data /= System.Null_Address then
         Free (Self.Data);
         Self.Data := System.Null_Address;
         Self.Ptr  := System.Null_Address;
      end if;
   end Finalize;

   procedure Allocate
     (Self : in out Bump_Ptr_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
      use type System.Address;

      Actual_Size : constant Storage_Count  := Size + 4;
   begin
      if Self.Data = System.Null_Address then
         --  First allocation
         Self.Data := Alloc (size_t (Data_Size));
         Self.Ptr  := Self.Data;
      end if;
      AGC.Register (Self.Ptr, Size);
      Addr := Self.Ptr + 4;
      Self.Ptr := Self.Ptr + Actual_Size;
   end Allocate;

   procedure Deallocate
     (Self : in out Bump_Ptr_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
   begin
      null;
   end Deallocate;
end AGC.Storage.Bump_Ptr;
