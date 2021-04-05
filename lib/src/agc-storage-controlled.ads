package AGC.Storage.Controlled is
   type Controlled_Pool is new AGC_Pool with null record;

   overriding procedure Collect
     (Self : in out Controlled_Pool; X : System.Address)
         with Inline;

   overriding function Is_Valid_Address
     (Self : in out Controlled_Pool; X : System.Address) return Boolean
         with Inline;

private

   overriding procedure Allocate
     (Self : in out Controlled_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
         with Inline;

   overriding procedure Deallocate
     (Self : in out Controlled_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count);

   overriding function Storage_Size
     (Self : Controlled_Pool) return Storage_Count is (Storage_Count'Last)
	      with Inline;
end AGC.Storage.Controlled;
