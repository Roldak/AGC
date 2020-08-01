with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package AGC.Storage.Bump_Ptr is
   type Bump_Ptr_Pool is new AGC_Pool with private;

   overriding procedure Collect
     (Self : in out Bump_Ptr_Pool; X : System.Address);

   overriding procedure Finalize (Self : in out Bump_Ptr_Pool);
private
   type Bump_Ptr_Pool is new AGC_Pool with record
      Data : System.Address := System.Null_Address;
      Ptr  : System.Address := System.Null_Address;
   end record;

   overriding procedure Allocate
     (Self : in out Bump_Ptr_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding procedure Deallocate
     (Self : in out Bump_Ptr_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding function Storage_Size
     (Self : Bump_Ptr_Pool) return Storage_Count is (Storage_Count'Last)
	      with Inline;
end AGC.Storage.Bump_Ptr;
