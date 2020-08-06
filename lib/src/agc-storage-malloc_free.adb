with System.Memory; use System.Memory;

with Ada.Unchecked_Conversion;

package body AGC.Storage.Malloc_Free is
   function Conv is new Ada.Unchecked_Conversion
     (System.Address, Long_Integer)
         with Inline;

   function Address_Hash
     (X : System.Address) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (Conv (X));
   end Address_Hash;

   procedure Collect
     (Self : in out Malloc_Free_Pool; X : System.Address)
   is
   begin
      Self.Allocated.Delete (X);
      Free (X);
   end Collect;

   function Is_Valid_Address
     (Self : in out Malloc_Free_Pool; X : System.Address) return Boolean
   is
   begin
      return Self.Allocated.Contains (X);
   end Is_Valid_Address;

   procedure Allocate
     (Self : in out Malloc_Free_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
      Actual_Size : constant Storage_Count := Size + 4;
      Allocated   : constant System.Address := Alloc (size_t (Actual_Size));
	begin
      AGC.Register (Allocated, Size);
      Self.Allocated.Insert (Allocated);
      Addr := Allocated + 4;
   end Allocate;

   procedure Deallocate
     (Self : in out Malloc_Free_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
   begin
      null;  --  Prevent explicit deallocation
   end Deallocate;
end AGC.Storage.Malloc_Free;
