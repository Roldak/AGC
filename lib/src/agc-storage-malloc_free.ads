with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package AGC.Storage.Malloc_Free is
   type Malloc_Free_Pool is new AGC_Pool with private;

   overriding procedure Collect
     (Self : in out Malloc_Free_Pool; X : System.Address);
private
   type Malloc_Free_Pool is new AGC_Pool with null record;

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
