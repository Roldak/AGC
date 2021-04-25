with System.Storage_Pools; use System.Storage_Pools;
with AGC.Storage.Dynamic;

package AGC.Storage.Get is
   AGC_Pool_Ptr : constant AGC_Pool_Access := Dynamic.Get_Pool;
   Pool_Ptr     : constant access Root_Storage_Pool'Class := AGC_Pool_Ptr;

   AGC_Pool : Storage.AGC_Pool'Class renames AGC_Pool_Ptr.all;
   Pool     : Root_Storage_Pool'Class renames Pool_Ptr.all;
end AGC.Storage.Get;
