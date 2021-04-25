with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package AGC.Storage is
   type AGC_Pool is abstract new Root_Storage_Pool with private;
   type AGC_Pool_Access is access all AGC_Pool'Class;

   procedure Collect
     (Self : in out AGC_Pool; X : System.Address) is abstract;

   function Is_Valid_Address
     (Self : in out AGC_Pool; X : System.Address) return Boolean is abstract;

   overriding procedure Finalize (Self : in out AGC_Pool) is null;

   Extra_Bytes : constant Storage_Count;

private

   type AGC_Pool is abstract new Root_Storage_Pool with null record;

   Default_Align : constant Storage_Count :=
      Standard'System_Allocator_Alignment;

   Header_Size : constant Storage_Count := 4;

   Extra_Bytes : constant Storage_Count :=
      ((Header_Size + Default_Align - 1) / Default_Align) * Default_Align;
end AGC.Storage;
