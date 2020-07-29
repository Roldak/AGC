with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with System; use System;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

package body GC is
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

   procedure Collect (Value : in Address)
      with Inline
   is
      Var : Address_Access := As_Address_Access (Value);
   begin
      Free (Var);
   end Collect;

   type Alloc_State is (Unknown, Reachable);
   type Alloc_State_Access is access all Alloc_State;
   for Alloc_State_Access'Size use Standard'Address_Size;

   function As_Alloc_State_Access is new
      Ada.Unchecked_Conversion (Address, Alloc_State_Access);

   type Root is record
      Addr    : Address;
      Visitor : Address;
   end record;

   package Address_Vectors is new Ada.Containers.Vectors (Positive, Address);
   package Root_Vectors is new Ada.Containers.Vectors (Positive, Root);

   Alloc_Set : Address_Vectors.Vector;
   Reach_Set : Root_Vectors.Vector;

   function Root_Count return Natural is (Natural (Reach_Set.Length));

   procedure Push_Root (X, Visitor : Address) is
   begin
      Reach_Set.Append ((X, Visitor));
   end Push_Root;

   procedure Pop_Roots (X : Natural) is
   begin
      Reach_Set.Set_Length (Ada.Containers.Count_Type (X));
   end Pop_Roots;

   procedure Register (X : Address) is
   begin
      Collect;
      Put_Line ("Adding " & Address_Image (X));
      Alloc_Set.Append (X);
      As_Alloc_State_Access (X).all := Unknown;
   end Register;

   procedure No_Op (X : Address) is null;

   procedure Visit_Access_Type (X : Address) is
      pragma Suppress (Accessibility_Check);

      type T_Access_Access is access all T_Access;
      for T_Access_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Access_Access);

      Acc : aliased T_Access := Conv (X).all;
   begin
      if Acc /= null then
         declare
            Elem_Addr   : Address := Acc.all'Address;
            Header_Addr : Address :=
               Elem_Addr - Storage_Offset
                 (T'Descriptor_Size / Storage_Unit
                  + Acc.all'Finalization_Size
                  + 4);
         begin
            As_Alloc_State_Access (Header_Addr).all := Reachable;
            Visit_Element (Elem_Addr);
         end;
      end if;
   end Visit_Access_Type;

   procedure Visit_Array_Type (X : Address) is
      pragma Suppress (Accessibility_Check);

      type T_Array is array (I range <>) of T;
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
   end Visit_Array_Type;

   procedure Collect is
   begin
      for Root of Reach_Set loop
         As_Address_Visitor (Root.Visitor).all (Root.Addr);
      end loop;

      for J in reverse 1 .. Alloc_Set.Last_Index loop
         declare
            Alloc : Address := Alloc_Set (J);
            State : Alloc_State_Access := As_Alloc_State_Access (Alloc);
         begin
            if State.all /= Unknown then
               Put_Line ("Keeping " & Address_Image (Alloc));
               State.all := Unknown;
            else
               Put_Line ("Collecting " & Address_Image (Alloc));
               Collect (Alloc);
               Alloc_Set.Delete (J);
            end if;
         end;
      end loop;
   end Collect;

   procedure Print_Stats is
   begin
      Put_Line ("Still alive : " & Alloc_Set.Length'Image);
   end Print_Stats;
end GC;
