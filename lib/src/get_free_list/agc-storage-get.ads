with System.Storage_Pools; use System.Storage_Pools;
with AGC.Storage.Free_List;

package AGC.Storage.Get is
   AGC_Pool : Free_List.Free_List_Pool;
   Pool     : Free_List.Free_List_Pool renames AGC_Pool;
end AGC.Storage.Get;
