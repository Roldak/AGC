with System.Storage_Pools; use System.Storage_Pools;
with AGC.Storage.Malloc_Free;

package AGC.Storage.Get is
   AGC_Pool : Malloc_Free.Malloc_Free_Pool;
   Pool     : Malloc_Free.Malloc_Free_Pool renames AGC_Pool;
end AGC.Storage.Get;
