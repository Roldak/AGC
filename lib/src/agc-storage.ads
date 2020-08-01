with System; use System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package AGC.Storage is
   type AGC_Pool is abstract new Root_Storage_Pool with private;
   type AGC_Pool_Access is access all AGC_Pool'Class;

   procedure Collect (P : in out AGC_Pool; X : Address) is abstract;

   function Get_Pool return AGC_Pool_Access;
private
   type AGC_Pool is abstract new Root_Storage_Pool with null record;
end AGC.Storage;
