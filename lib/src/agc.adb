with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with System; use System;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

with AGC.Storage.Get;
with AGC.Validate_Addresses;
with AGC.Vectors;

package body AGC is
   type Address_Access is access all Address;
   type Address_Visitor is access procedure (X : Address);

   function As_Address_Access is new Ada.Unchecked_Conversion
     (Address, Address_Access)
         with Inline;

   function As_Address_Visitor is new Ada.Unchecked_Conversion
     (Address, Address_Visitor)
         with Inline;

   procedure Free is new Ada.Unchecked_Deallocation
     (Address, Address_Access)
         with Inline;

   type Alloc_State is (Unknown, Reachable);
   type Alloc_State_Access is access all Alloc_State;
   for Alloc_State_Access'Size use Standard'Address_Size;

   function As_Alloc_State_Access is new
      Ada.Unchecked_Conversion (Address, Alloc_State_Access)
         with Inline;

   type Alloc is record
      Header : Address;
      Object : Address;
      Final  : Finalizer;
   end record;

   type Root is record
      Addr    : Address;
      Visitor : Address;
   end record;

   package Alloc_Vectors is new AGC.Vectors (Alloc);
   package Address_Vectors is new AGC.Vectors (Address);
   package Root_Vectors is new AGC.Vectors (Root);

   Alloc_Set : Alloc_Vectors.Vector;
   Modif_Set : Address_Vectors.Vector;
   Reach_Set : Root_Vectors.Vector;

   Current_Size : Storage_Count := 0;
   Max_Size     : constant Storage_Count := 1024 * 1024 * 2;

   function Root_Count return Natural is (Reach_Set.Length);

   function Push_Root
     (X, Visitor : System.Address) return Empty_Type
   is
   begin
      Reach_Set.Append ((X, Visitor));
      return (null record);
   end Push_Root;

   procedure Pop_Roots (X : Natural) is
   begin
      Reach_Set.Set_Length (X);
   end Pop_Roots;

   Total_Registered : Natural := 0;
   Any_Modified     : Boolean := False;

   procedure Register
     (Header_Address : System.Address;
      Object_Address : System.Address;
      Size           : System.Storage_Elements.Storage_Count;
      Final          : Finalizer)
   is
   begin
      if Current_Size > Max_Size then
         Collect;
         Current_Size := 0;
      end if;

      Current_Size := Current_Size + Size;
      Total_Registered := Total_Registered + 1;

      Alloc_Set.Append (Alloc'(Header_Address, Object_Address, Final));
      As_Alloc_State_Access (Header_Address).all := Unknown;
   end Register;

   package body Access_Type_Operations is
      pragma Suppress (All_Checks);

      function Register
        (Acc : Named_Access) return Named_Access
      is
         Finalization_Size : constant Storage_Count :=
            Storage_Count (Integer'(Acc.all'Finalization_Size));
         --  Workaround of a weird GNAT bug where 'Finalization_Size
         --  doesn't seem to take the right type when used inline.

         Type_Offset : constant Storage_Offset :=
            T'Descriptor_Size / Storage_Unit + Finalization_Size;

         Object_Address : constant Address := Acc.all'Address;

         Header_Address : constant Address :=
            Object_Address - Type_Offset - Storage.Extra_Bytes;

         Final : Finalizer :=
           (if Finalization_Size > 0
            then Finalize_From_Object_Address'Unrestricted_Access
            else null);

         Size : constant Storage_Count :=
            T'Object_Size / Storage_Unit + Storage.Extra_Bytes;
      begin
         Register (Header_Address, Object_Address, Size, Final);
         return Acc;
      end Register;

      type T_Access is access all T
         with Storage_Pool => Storage.Get.AGC_Pool;

      type T_Access_Access is access all T_Access;
      for T_Access_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Access_Access);

      procedure Visit_Access_Type (X : Address) is
         Acc : aliased constant T_Access := Conv (X).all;
      begin
         if Acc /= null then
            Visit_Element (Acc.all'Address);
         end if;
      end Visit_Access_Type;

      procedure Mark_And_Visit_Access_Type (X : Address) is
         Acc : aliased constant T_Access := Conv (X).all;
      begin
         if Acc /= null then
            declare
               Finalization_Size : constant Storage_Count :=
                  Storage_Count (Integer'(Acc.all'Finalization_Size));
               --  Workaround of a weird GNAT bug where 'Finalization_Size
               --  doesn't seem to take the right type when used inline.

               Type_Offset : constant Storage_Offset :=
                  T'Descriptor_Size / Storage_Unit + Finalization_Size;

               Object_Address : constant Address := Acc.all'Address;

               Header_Address : constant Address :=
                  Object_Address - Type_Offset - Storage.Extra_Bytes;

               State : Alloc_State_Access :=
                  As_Alloc_State_Access (Header_Address);
            begin
               if Validate_Addresses.Value and then
                  not Storage.Get.AGC_Pool.Is_Valid_Address (Header_Address)
               then
                  return;
               end if;

               if State.all = Unknown then
                  State.all := Reachable;
                  Visit_Element (Object_Address);

                  if not Validate_Addresses.Value
                     and then Is_Generalized_Access
                  then
                     Any_Modified := True;
                     Modif_Set.Append (Header_Address);
                  end if;
               end if;
            end;
         end if;
      end Mark_And_Visit_Access_Type;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (T, T_Access);

      procedure Finalize_From_Object_Address
        (Object_Address : System.Address)
      is
         Acc : T_Access := Conv (Object_Address);
      begin
         Free (Acc);
      end Finalize_From_Object_Address;
   end Access_Type_Operations;

   procedure Visit_Constrained_Array_1_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access all T_Array;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range loop
         declare
            C : aliased T := Arr.all (I);
         begin
            Visit_Element (C'Address);
         end;
      end loop;
   end Visit_Constrained_Array_1_Type;

   procedure Visit_Unconstrained_Array_1_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access all T_Array;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range loop
         declare
            C : aliased T := Arr.all (I);
         begin
            Visit_Element (C'Address);
         end;
      end loop;
   end Visit_Unconstrained_Array_1_Type;

   procedure Visit_Constrained_Array_2_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access all T_Array;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range (1) loop
         for J in Arr.all'Range (2) loop
            declare
               C : aliased T := Arr.all (I, J);
            begin
               Visit_Element (C'Address);
            end;
         end loop;
      end loop;
   end Visit_Constrained_Array_2_Type;

   procedure Visit_Unconstrained_Array_2_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access all T_Array;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range (1) loop
         for J in Arr.all'Range (2) loop
            declare
               C : aliased T := Arr.all (I, J);
            begin
               Visit_Element (C'Address);
            end;
         end loop;
      end loop;
   end Visit_Unconstrained_Array_2_Type;

   procedure Reset_Modified is
   begin
      for Alloc of Modif_Set loop
         As_Alloc_State_Access (Alloc).all := Unknown;
      end loop;
      Modif_Set.Clear;
      Any_Modified := False;
   end Reset_Modified;

   New_Set : Alloc_Vectors.Vector;

   procedure Collect is
   begin
      for Root of Reach_Set loop
         As_Address_Visitor (Root.Visitor).all (Root.Addr);
      end loop;

      for Alloc of Alloc_Set loop
         declare
            State : Alloc_State_Access := As_Alloc_State_Access (Alloc.Header);
         begin
            if State.all = Reachable then
               State.all := Unknown;
               New_Set.Append (Alloc);
            else
               if Alloc.Final /= null then
                  Alloc.Final (Alloc.Object);
               end if;
               AGC.Storage.Get.AGC_Pool.Collect (Alloc.Header);
            end if;
         end;
      end loop;

      if not Validate_Addresses.Value and Any_Modified then
         Reset_Modified;
      end if;

      Alloc_Vectors.Move (Alloc_Set, New_Set);
   end Collect;

   procedure Print_Stats is
   begin
      Put_Line ("Still alive : " & Alloc_Set.Length'Image);
      Put_Line ("Total registered : " & Total_Registered'Image);
      Put_Line ("Root count : " & Reach_Set.Length'Image);
   end Print_Stats;

begin
   Alloc_Set.Reserve (10);
   Modif_Set.Reserve (10);
   Reach_Set.Reserve (10);
end AGC;
