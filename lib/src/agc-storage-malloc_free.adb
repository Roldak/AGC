with System.Memory; use System.Memory;

package body AGC.Storage.Malloc_Free is
   procedure Collect
     (Self : in out Malloc_Free_Pool; X : System.Address)
   is
   begin
      Free (X);
   end Collect;

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
