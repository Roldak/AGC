with System.Memory; use System.Memory;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with AGC.Storage.Malloc_Free;
with AGC.Storage.Bump_Ptr;

package body AGC.Storage is
   procedure Free is new Ada.Unchecked_Deallocation
     (AGC_Pool'Class, AGC_Pool_Access);

   type Pool_Holder is new Ada.Finalization.Controlled with record
      Pool : AGC_Pool_Access := null;
   end record;

   overriding procedure Finalize (X : in out Pool_Holder) is
   begin
      if X.Pool /= null then
         X.Pool.Finalize;
         Free (X.Pool);
      end if;
   end Finalize;

   Current_Pool : Pool_Holder;

   function Get_Pool return AGC_Pool_Access is
   begin
      if Current_Pool.Pool = null then
         if Exists ("AGC_POOL") then
            declare
               V : String := Value ("AGC_POOL");
            begin
               Put_Line (V);
               if V = "MALLOC_FREE" then
                  Current_Pool.Pool := new Malloc_Free.Malloc_Free_Pool;
               elsif V = "BUMP_PTR" then
                  Current_Pool.Pool := new Bump_Ptr.Bump_Ptr_Pool;
               else
                  raise Program_Error with "Unknown Storage Pool";
               end if;
            end;
         else
            Current_Pool.Pool := new Malloc_Free.Malloc_Free_Pool;
         end if;
      end if;

      return Current_Pool.Pool;
   end Get_Pool;
end AGC.Storage;
