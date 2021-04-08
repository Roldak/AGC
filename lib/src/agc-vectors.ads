with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

--  This package implements a very simple Vector type. It has the following
--  attributes:
--
--  - Very lightweight implementation, very few primitives.
--  - Not controlled (manual memory management).
--  - Ada 2012-like iteration via the Iterate aspect, so read-only access to
--    elements in for .. of loops.
--  - Uses realloc for resize, so faster, but won't be correct on every type.
--  - Not tagged, so no dot notation on primitives.
--  - Small vector optimization: can store a number of the elements inline.

generic
   type Element_Type is private;
package AGC.Vectors is

   subtype Index_Type is Positive;
   type Elements_Array is array (Index_Type range <>) of Element_Type;

   subtype Iteration_Index_Type is Natural;
   --  Like Index_Type, but also covers zero, which is used to represent a
   --  dummy last index value for empty vectors.

   type Vector is tagged private
     with Iterable =>
       (First       => First_Index,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Get);

   Empty_Vector : constant Vector;

   function Is_Empty (Self : Vector) return Boolean
     with Inline;
   --  Return whether Self is an empty vector

   procedure Append (Self : in out Vector; Element : Element_Type)
     with Inline;
   --  Appends Element to Self

   procedure Reserve (Self : in out Vector; Capacity : Natural)
     with Inline;
   --  Make sure that Self has enough room to contain Capacity elements in
   --  total.

   function Get
     (Self : Vector; Index : Iteration_Index_Type) return Element_Type
     with Inline;
   --  Get the element at Index

   procedure Set (Self : in out Vector; Index : Index_Type; E : Element_Type)
     with Inline;
   --  Set the element at Index to E

   procedure Destroy (Self : in out Vector)
     with Inline;
   --  Destroy this vector

   procedure Clear (Self : in out Vector)
     with Inline;
   --  Remove every element in this vector.
   --  NOTICE: this function does not actually free the memory of the vector!

   function First_Element (Self : Vector) return Element_Type;
   --  Return the first element in this vector

   function Last_Element (Self : Vector) return Element_Type;
   --  Return the last element in this vector

   function Length (Self : Vector) return Natural
     with Inline;
   --  Return the Length of the vector, ie. the number of elements it contains

   procedure Set_Length (Self : in out Vector; N : Natural)
     with Inline;
   --  Resize the given vector so that it holes N elements

   function First_Index (Self : Vector) return Iteration_Index_Type
   is (Index_Type'First)
     with Inline;
   --  Return the first index, only used for the Iterable aspect

   function Last_Index (Self : Vector) return Iteration_Index_Type
   is (First_Index (Self) + Length (Self) - 1)
     with Inline;
   --  Return the index of the last element in this vector or
   --  First_Index (Self) - 1 if this vector is empty.

   function Next
     (Self : Vector; N : Iteration_Index_Type) return Iteration_Index_Type
   is (N + 1)
   with Inline;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect.

   function Previous
     (Self : Vector; N : Iteration_Index_Type) return Iteration_Index_Type
   is (N - 1)
     with Inline;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect.

   function Has_Element
     (Self : Vector; N : Iteration_Index_Type) return Boolean
   is (N in First_Index (Self) .. Last_Index (Self))
     with Inline;
   --  Given a vector and an index, return True if the index is in the vector
   --  range. Only used for the iterable aspect.

   procedure Move (Target : in out Vector; Source : in out Vector)
     with Inline;
   -- Move the internal elements array of vector Source to vector Target.
   -- Clear the source vector.

private

   subtype Internal_Elements_Array is Elements_Array (Index_Type);
   type Elements_Array_Access is access all Internal_Elements_Array;

   function To_Pointer is
     new Ada.Unchecked_Conversion (System.Address, Elements_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Elements_Array, Elements_Array_Access);

   type Vector is tagged record
      E        : Elements_Array_Access := null;
      Size     : Natural := 0;
      Capacity : Natural := 0;
   end record;

   Empty_Vector : constant Vector := (E => null, Size => 0, Capacity => 0);

end AGC.Vectors;

