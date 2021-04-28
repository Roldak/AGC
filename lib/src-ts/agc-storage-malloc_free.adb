with System.Memory; use System.Memory;

with Ada.Unchecked_Conversion;

package body AGC.Storage.Malloc_Free is
   function Address_Hash
     (X : System.Address) return Ada.Containers.Hash_Type
   is
      pragma Warnings
        (Off, "types for unchecked conversion have different sizes");
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Ada.Containers.Hash_Type)
            with Inline;
      pragma Warnings
        (On, "types for unchecked conversion have different sizes");
   begin
      return Conv (X);
   end Address_Hash;

   procedure Lock (Self : in out Malloc_Free_Pool) is
   begin
      loop
         exit when Self.In_Use.Compare_And_Swap (False, True);
      end loop;
   end Lock;

   procedure Unlock (Self : in out Malloc_Free_Pool) is
   begin
      Self.In_Use.Set (False);
   end Unlock;

   procedure Collect
     (Self : in out Malloc_Free_Pool; X : System.Address)
   is
   begin
      Free (X);
      begin
         Lock (Self);
         Self.Allocated.Delete (X);
         Unlock (Self);
      exception
         when others =>
            Unlock (Self);
            raise;
      end;
   end Collect;

   function Is_Valid_Address
     (Self : in out Malloc_Free_Pool; X : System.Address) return Boolean
   is
      Result : Boolean;
   begin
      Lock (Self);
      Result := Self.Allocated.Contains (X);
      Unlock (Self);
      return Result;
   exception
      when others =>
         Unlock (Self);
         raise;
   end Is_Valid_Address;

   procedure Allocate
     (Self : in out Malloc_Free_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
      Actual_Size : constant Storage_Count := Size + Extra_Bytes;
      Allocated   : constant System.Address := Alloc (size_t (Actual_Size));
	begin
      Addr := Allocated + Extra_Bytes;
      begin
         Lock (Self);
         Self.Allocated.Insert (Allocated);
         Unlock (Self);
      exception
         when others =>
            Unlock (Self);
            raise;
      end;
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
