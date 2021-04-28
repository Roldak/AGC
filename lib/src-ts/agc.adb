with Ada.Text_IO; use Ada.Text_IO;

with Ada.Finalization;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with Ada.Exceptions;

with Ada.Task_Attributes;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Task_Termination;

with System; use System;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

with AGC.Storage.Get;
with AGC.Vectors;
with AGC.Roots; use AGC.Roots;
with AGC.Task_States; use AGC.Task_States;

package body AGC is
   type Address_Access is access all Address;

   function As_Address_Access is new Ada.Unchecked_Conversion
     (Address, Address_Access)
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

   package Alloc_Vectors is new AGC.Vectors (Alloc);
   package Address_Vectors is new AGC.Vectors (Address);
   package Task_Vectors is new AGC.Vectors (Task_Id);

   Max_Size : constant Storage_Count := 1024 * 1024 * 2;

   type Task_State_Access is access Task_State_Record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Task_State_Record, Task_State_Access);

   package Task_State is new Ada.Task_Attributes (Task_State_Access, null);

   protected Global_State is
      procedure Init;
      procedure Finalize;

      procedure Register_Task (X : Task_Id);
      procedure Unregister_Task
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence);

      procedure Register_Alloc
        (Header_Address : System.Address;
         Object_Address : System.Address;
         Size           : System.Storage_Elements.Storage_Count;
         Final          : Finalizer);

      procedure Collect;
      procedure Print_Stats;
   private
      Task_Set  : Task_Vectors.Vector;
      Alloc_Set : Alloc_Vectors.Vector;
      New_Set   : Alloc_Vectors.Vector;

      Current_Size     : Storage_Count := 0;
      Total_Registered : Natural := 0;
   end Global_State;

   function Self_State return Task_State_Access is
      X : Task_State_Access := Task_State.Value;
   begin
      if X = null then
         X := new Task_State_Record;
         Global_State.Register_Task (Current_Task);
         Ada.Task_Termination.Set_Specific_Handler
           (Current_Task, Global_State.Unregister_Task'Access);
         Task_State.Set_Value (X);
         X.Init;
      end if;
      return X;
   end Self_State;

   function Root_Count return Natural is
     (Self_State.Root_Count);

   function Push_Root
     (X, Visitor : System.Address) return Empty_Type
   is
   begin
      Self_State.Add_Root ((X, Visitor));
      return (null record);
   end Push_Root;

   procedure Pop_Roots (X : Natural) is
   begin
      Self_State.Pop_Roots (X);
   end Pop_Roots;

   protected body Global_State is
      procedure Init is
      begin
         Alloc_Set.Reserve (10);
      end Init;

      procedure Finalize is
      begin
         Alloc_Set.Destroy;
         New_Set.Destroy;
         for T of Task_Set loop
            Task_State.Value (T).Finalize;
            Free (Task_State.Reference (T).all);
         end loop;
         Task_Set.Destroy;
      end Finalize;

      procedure Register_Task (X : Task_Id) is
      begin
         Task_Set.Append (X);
      end Register_Task;

      procedure Unregister_Task
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence)
      is
         pragma Unreferenced (Cause, X);
      begin
         Task_State.Value (T).Finalize;
         Free (Task_State.Reference (T).all);
         for I in 1 .. Task_Set.Length loop
            if T = Task_Set.Get (I) then
               Task_Set.Swap_And_Remove (I);
               exit;
            end if;
         end loop;
      end Unregister_Task;

      procedure Register_Alloc
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
      end Register_Alloc;

      procedure Collect is
      begin
         for T of Task_Set loop
            Task_State.Value (T).Visit_Roots;
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

         Alloc_Vectors.Move (Alloc_Set, New_Set);
      end Collect;

      procedure Print_Stats is
      begin
         Put_Line ("Still alive : " & Alloc_Set.Length'Image);
         Put_Line ("Total registered : " & Total_Registered'Image);
         Put_Line ("Task count : " & Task_Set.Length'Image);
      end Print_Stats;
   end Global_State;

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
            Acc.all'Size / Storage_Unit + Storage.Extra_Bytes;
      begin
         Global_State.Register_Alloc
           (Header_Address, Object_Address, Size, Final);
         return Acc;
      end Register;

      type T_Access is access T
         with Storage_Pool => Storage.Get.AGC_Pool;

      type T_Access_Access is access T_Access
         with Storage_Size => 0;
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
               if
                  Storage.Get.AGC_Pool.Is_Valid_Address (Header_Address)
                  and then State.all = Unknown
               then
                  State.all := Reachable;
                  Visit_Element (Object_Address);
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

      type T_Array_Access is access T_Array
         with Storage_Size => 0;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range loop
         Visit_Element (Arr.all (I)'Address);
      end loop;
   end Visit_Constrained_Array_1_Type;

   procedure Visit_Unconstrained_Array_1_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access T_Array
         with Storage_Size => 0;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range loop
         Visit_Element (Arr.all (I)'Address);
      end loop;
   end Visit_Unconstrained_Array_1_Type;

   procedure Visit_Constrained_Array_2_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access T_Array
         with Storage_Size => 0;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range (1) loop
         for J in Arr.all'Range (2) loop
            Visit_Element (Arr.all (I, J)'Address);
         end loop;
      end loop;
   end Visit_Constrained_Array_2_Type;

   procedure Visit_Unconstrained_Array_2_Type (X : Address) is
      pragma Suppress (All_Checks);

      type T_Array_Access is access T_Array
         with Storage_Size => 0;
      for T_Array_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (Address, T_Array_Access);

      Arr : T_Array_Access := Conv (X);
   begin
      for I in Arr.all'Range (1) loop
         for J in Arr.all'Range (2) loop
            Visit_Element (Arr.all (I, J)'Address);
         end loop;
      end loop;
   end Visit_Unconstrained_Array_2_Type;

   procedure Collect is
   begin
      Global_State.Collect;
   end Collect;

   procedure Print_Stats is
   begin
      Global_State.Print_Stats;
   end Print_Stats;

   package Data_Finalizers is
      type Finalizer is new Ada.Finalization.Controlled with null record;

      overriding procedure Finalize (X : in out Finalizer);
   end Data_Finalizers;

   package body Data_Finalizers is
      overriding procedure Finalize (X : in out Finalizer) is
      begin
         Global_State.Finalize;
      end Finalize;
   end Data_Finalizers;

   Data_Finalizer : Data_Finalizers.Finalizer;
begin
   Global_State.Init;
end AGC;
