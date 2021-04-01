with System.Memory; use System.Memory;

with Ada.Unchecked_Conversion;

with AGC.Validate_Addresses;

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
      Free (X);

      if Validate_Addresses.Value then
         Self.Allocated.Delete (X);
      end if;
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
      Actual_Size : constant Storage_Count := Size + Extra_Bytes;
      Allocated   : constant System.Address := Alloc (size_t (Actual_Size));
	begin
      AGC.Register (Allocated, Size);
      Addr := Allocated + Extra_Bytes;

      if Validate_Addresses.Value then
         Self.Allocated.Insert (Allocated);
      end if;
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
