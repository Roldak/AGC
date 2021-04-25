with System.Memory; use System.Memory;

with Ada.Unchecked_Conversion;

package body AGC.Storage.Malloc_Free is
   function Address_Hash
     (X : System.Address) return Ada.Containers.Hash_Type
   is
      pragma Warnings
        (Off, "types for unchecked conversion have different sizes");
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Ada.Containers.Hash_Type)
            with Inline;
      pragma Warnings
        (On, "types for unchecked conversion have different sizes");
   begin
      return Conv (X);
   end Address_Hash;

   protected body Address_Set is
      procedure Insert (X : System.Address) is
      begin
         Set.Insert (X);
      end Insert;

      procedure Delete (X : System.Address) is
      begin
         Set.Delete (X);
      end Delete;

      function Contains (X : System.Address) return Boolean is
      begin
         return Set.Contains (X);
      end Contains;
   end Address_Set;

   procedure Collect
     (Self : in out Malloc_Free_Pool; X : System.Address)
   is
   begin
      Free (X);
      Self.Allocated.Delete (X);
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
      Addr := Allocated + Extra_Bytes;
      Self.Allocated.Insert (Allocated);
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
