with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package AGC.Storage.Free_List is
   type Free_List_Pool is new AGC_Pool with private;

   overriding procedure Collect
     (Self : in out Free_List_Pool; X : System.Address)
         with Inline;

   function Is_Valid_Address
     (Self : in out Free_List_Pool; X : System.Address) return Boolean
         with Inline;

   overriding procedure Finalize (Self : in out Free_List_Pool);
private
   type Free_Info is record
      Size : Storage_Count;
      Prev : System.Address;
      Next : System.Address;
   end record;

   type Free_List_Pool is new AGC_Pool with record
      Data : System.Address := System.Null_Address;
      Free : System.Address := System.Null_Address;
   end record;

   overriding procedure Allocate
     (Self : in out Free_List_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
         with Inline;

   overriding procedure Deallocate
     (Self : in out Free_List_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding function Storage_Size
     (Self : Free_List_Pool) return Storage_Count is (Storage_Count'Last)
	      with Inline;
end AGC.Storage.Free_List;
