with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

with Ada.Containers.Hashed_Sets;

package AGC.Storage.Malloc_Free is
   type Malloc_Free_Pool is new AGC_Pool with private;

   overriding procedure Collect
     (Self : in out Malloc_Free_Pool; X : System.Address);

   overriding function Is_Valid_Address
     (Self : in out Malloc_Free_Pool; X : System.Address) return Boolean;
private
   function Address_Hash
     (X : System.Address) return Ada.Containers.Hash_Type
         with Inline;

   package Address_Sets is new Ada.Containers.Hashed_Sets
     (System.Address, Address_Hash, System."=", System."=");

   type Malloc_Free_Pool is new AGC_Pool with record
      Allocated : Address_Sets.Set;
   end record;

   overriding procedure Allocate
     (Self : in out Malloc_Free_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding procedure Deallocate
     (Self : in out Malloc_Free_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding function Storage_Size
     (Self : Malloc_Free_Pool) return Storage_Count is (Storage_Count'Last)
	      with Inline;
end AGC.Storage.Malloc_Free;
