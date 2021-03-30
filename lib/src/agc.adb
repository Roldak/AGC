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

   type Root is record
      Addr    : Address;
      Visitor : Address;
   end record;

   package Address_Vectors is new Ada.Containers.Vectors (Positive, Address);
   package Root_Vectors is new Ada.Containers.Vectors (Positive, Root);

   Alloc_Set : Address_Vectors.Vector;
   Modif_Set : Address_Vectors.Vector;
   Reach_Set : Root_Vectors.Vector;

   Current_Size : Storage_Count := 0;
   Max_Size     : constant Storage_Count := 1024 * 1024 * 2;

   function Root_Count return Natural is (Natural (Reach_Set.Length));

   function Push_Root
     (X, Visitor : System.Address) return Empty_Type
   is
   begin
      Reach_Set.Append ((X, Visitor));
      return (null record);
   end Push_Root;

   procedure Pop_Roots (X : Natural) is
   begin
      Reach_Set.Set_Length (Ada.Containers.Count_Type (X));
   end Pop_Roots;

   Total_Registered : Natural := 0;

   procedure Register
     (Addr : Address;
      Size : Storage_Count)
   is
   begin
      if Current_Size > Max_Size then
         Collect;
         Current_Size := 0;
      end if;
      Current_Size := Current_Size + Size;
      Total_Registered := Total_Registered + 1;

      Alloc_Set.Append (Addr);
      As_Alloc_State_Access (Addr).all := Unknown;
   end Register;

   Any_Modified : Boolean := False;

   procedure Visit_Access_Type (X : Address) is
      pragma Suppress (Accessibility_Check);

      type T_Access is access all T;
      type T_Access_Access is access all T_Access;
      for T_Access_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Access_Access);

      Acc         : aliased constant T_Access := Conv (X).all;
   begin
      if Acc /= null then
         declare
            Finalization_Size : constant Storage_Offset :=
               Storage_Offset (Integer'(Acc.all'Finalization_Size));
            --  Workaround of a weird GNAT bug where 'Finalization_Size
            --  doesn't seem to take the right type when used inline.

            Type_Offset : constant Storage_Offset :=
               T'Descriptor_Size / Storage_Unit + Finalization_Size;

            Elem_Addr   : constant Address := Acc.all'Address;
            Header_Addr : constant Address := Elem_Addr - Type_Offset - 4;

            State : Alloc_State_Access := As_Alloc_State_Access (Header_Addr);
         begin
            if Validate_Addresses.Value
               and then not Storage.Get.AGC_Pool.Is_Valid_Address (Header_Addr)
            then
               return;
            end if;

            if State.all = Unknown then
               State.all := Reachable;
               Visit_Element (Elem_Addr);

               if not Validate_Addresses.Value
                  and then Is_Generalized_Access
               then
                  Any_Modified := True;
                  Modif_Set.Append (Header_Addr);
               end if;
            end if;
         end;
      end if;
   end Visit_Access_Type;

   procedure Visit_Constrained_Array_1_Type (X : Address) is
      pragma Suppress (Accessibility_Check);

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
      pragma Suppress (Accessibility_Check);

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
      pragma Suppress (Accessibility_Check);

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
      pragma Suppress (Accessibility_Check);

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

   New_Set : Address_Vectors.Vector;

   procedure Collect is
   begin
      for Root of Reach_Set loop
         As_Address_Visitor (Root.Visitor).all (Root.Addr);
      end loop;

      for Alloc of Alloc_Set loop
         declare
            State : Alloc_State_Access := As_Alloc_State_Access (Alloc);
         begin
            if State.all = Reachable then
               State.all := Unknown;
               New_Set.Append (Alloc);
            else
               AGC.Storage.Get.AGC_Pool.Collect (Alloc);
            end if;
         end;
      end loop;

      if not Validate_Addresses.Value and Any_Modified then
         Reset_Modified;
      end if;

      Address_Vectors.Move (Alloc_Set, New_Set);
   end Collect;

   procedure Print_Stats is
   begin
      Put_Line ("Still alive : " & Alloc_Set.Length'Image);
      Put_Line ("Total registered : " & Total_Registered'Image);
   end Print_Stats;
end AGC;
