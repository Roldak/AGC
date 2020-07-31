with System; use System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package AGC.Storage is
   type GC_Pool is new Root_Storage_Pool with null record;

   overriding procedure Allocate
     (Self : in out GC_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding procedure Deallocate
     (Self : in out GC_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding function Storage_Size
     (Self : GC_Pool) return Storage_Count is (Storage_Count'Last)
	      with Inline;

   Pool : GC_Pool;
end AGC.Storage;
