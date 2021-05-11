-- 45ef8ac8a2d466b1000b103f2895a569f30d0994
with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
procedure Test is
   AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
   type Integer_Access is access Integer;

   for Integer_Access'Storage_Pool use AGC.Storage.Get.Pool;
   procedure AGC_Visit_Integer_Access (X : System.Address) with
      Inline;
   package AGC_Integer_Access_Ops_Implem is new AGC.Access_Type_Operations
     (Standard.Integer, Integer_Access, False, AGC.No_Op);
   procedure AGC_Visit_Integer_Access (X : System.Address) renames
     AGC_Integer_Access_Ops_Implem.Mark_And_Visit_Access_Type;
   function AGC_Register_Integer_Access
     (X : Integer_Access) return Integer_Access with
      Inline;
   function AGC_Register_Integer_Access
     (X : Integer_Access) return Integer_Access renames
     AGC_Integer_Access_Ops_Implem.Register;
   X : aliased Integer_Access :=
     Test.AGC_Register_Integer_Access (Test.Integer_Access'(new Integer'(3)));
   AGC_Dummy_0 : constant AGC.Empty_Type :=
     AGC.Push_Root (X'Address, Test.AGC_Visit_Integer_Access'Address);
   Y          : Integer        := X.all;
   AGC_Free_X : AGC.Empty_Type := AGC.Free (X);
begin
   AGC.Pop_Roots (AGC_Base_Root_Count);
end Test;
