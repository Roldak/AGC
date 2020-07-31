with Ada.Text_IO; use Ada.Text_IO;
with System.Memory; use System.Memory;

package body AGC.Storage is
   procedure Allocate
     (Self : in out GC_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
		pragma Unreferenced (Self, Alignment);
      Actual_Size : constant Storage_Count := Size + 4;
      Allocated   : constant Address := Alloc (size_t (Actual_Size));
	begin
      AGC.Register (Allocated);
      Addr := Allocated + 4;
   end Allocate;

   procedure Deallocate
     (Self : in out GC_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
   begin
      null;
   end Deallocate;
end AGC.Storage;
