with System;        use System;
with System.Memory; use System.Memory;

-----------------
-- AGC.Vectors --
-----------------

package body AGC.Vectors is

   El_Size : constant size_t := Elements_Array'Component_Size / Storage_Unit;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Vector) return Boolean
   is (Self.Size = 0);

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Self : in out Vector; Capacity : Natural) is
      Siz : constant size_t := size_t (Capacity) * El_Size;
   begin
      if Self.E = null then
         --  E is null: First alloc
         Self.E := To_Pointer (Alloc (Siz));
      else
         --  E is not null: realloc
         Self.E := To_Pointer (Realloc (Self.E.all'Address, Siz));
      end if;

      Self.Capacity := Capacity;
   end Reserve;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Element : Element_Type) is
   begin
      if Self.Capacity = Self.Size then
         Reserve (Self, (Self.Capacity * 2) + 1);
      end if;
      Self.Size := Self.Size + 1;
      Self.E.all (Self.Size) := Element;
   end Append;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Vector; Index : Iteration_Index_Type) return Element_Type
   is (Self.E (Index));

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Vector; Index : Index_Type; E : Element_Type)
   is
   begin
      Self.E (Index) := E;
   end Set;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Vector) is
   begin
      Free (Self.E);
   end Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector) is
   begin
      Self.Size := 0;
   end Clear;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Self : Vector) return Element_Type
   is (Get (Self, First_Index (Self)));

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Self : Vector) return Element_Type
   is
   begin
      return Get (Self, Last_Index (Self));
   end Last_Element;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is (Self.Size);

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Self : in out Vector; N : Natural) is
   begin
      Self.Size := N;
   end Set_Length;

   ---------------------
   -- Swap_And_Remove --
   ---------------------

   procedure Swap_And_Remove (Self : in out Vector; I : Positive) is
   begin
      Self.E.all (I) := Self.E.all (Self.Size);
      Self.Size := Self.Size - 1;
   end Swap_And_Remove;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Vector; Source : in out Vector) is
      Target_E : constant Elements_Array_Access := Target.E;
      Target_C : constant Natural := Target.Capacity;
   begin
      Target.E := Source.E;
      Source.E := Target_E;

      Target.Capacity := Source.Capacity;
      Target.Size := Source.Size;

      Source.Capacity := Target_C;
      Source.Size := 0;
   end Move;

end AGC.Vectors;

