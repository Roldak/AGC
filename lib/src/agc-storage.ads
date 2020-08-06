with System;
with System.Storage_Pools; use System.Storage_Pools;

package AGC.Storage is
   type AGC_Pool is abstract new Root_Storage_Pool with private;
   type AGC_Pool_Access is access all AGC_Pool'Class;

   procedure Collect
     (Self : in out AGC_Pool; X : System.Address) is abstract;

   function Is_Valid_Address
     (Self : in out AGC_Pool; X : System.Address) return Boolean is abstract;

   overriding procedure Finalize (Self : in out AGC_Pool) is null;
private
   type AGC_Pool is abstract new Root_Storage_Pool with null record;
end AGC.Storage;
