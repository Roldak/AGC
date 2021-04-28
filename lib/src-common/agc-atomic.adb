package body AGC.Atomic is
   use type Interfaces.Integer_8;

   procedure Set (Self : in out Atomic_Boolean; Value : Boolean) is
   begin
      Self.Value := (if Value then 1 else 0);
   end Set;

   function Get (Self : Atomic_Boolean) return Boolean is
   begin
      return Self.Value = 1;
   end Get;

   function Compare_And_Swap
     (Self  : in out Atomic_Boolean;
      Test  : Boolean;
      Value : Boolean) return Boolean
   is
      function Intrinsic_Sync_Bool_Compare_And_Swap
        (Ptr    : access Interfaces.Integer_8;
         Oldval : Interfaces.Integer_8;
         Newval : Interfaces.Integer_8) return Boolean;
      pragma Import (Intrinsic, Intrinsic_Sync_Bool_Compare_And_Swap,
                     External_Name => "__sync_bool_compare_and_swap_1");
   begin
      return Intrinsic_Sync_Bool_Compare_And_Swap
        (Self.Value'Access,
         (if Test then 1 else 0),
         (if Value then 1 else 0));
   end Compare_And_Swap;
end AGC.Atomic;
