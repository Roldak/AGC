with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with System;

package AGC.Standard is
   --  Ada.Containers.Doubly_Linked_Lists
   generic
      with package Lists is new Ada.Containers.Doubly_Linked_Lists (<>);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Doubly_Linked_Lists_Visitors is
      procedure AGC_Visit_List_Private (X : System.Address)
         with Inline;
      procedure AGC_Visit_Cursor_Private
        (X : System.Address) is null;
   end Ada_Containers_Doubly_Linked_Lists_Visitors;

   --  Ada.Containers.Indefinite_Vectors
   generic
      with package Vectors is new Ada.Containers.Indefinite_Vectors (<>);
      with procedure Visit_Index_Type (X : System.Address);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Indefinite_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Indefinite_Vectors_Visitors;

   -- Ada.Containers.Hashed_Sets
   generic
      with package Sets is new Ada.Containers.Hashed_Sets (<>);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Hashed_Sets_Visitors is
      procedure AGC_Visit_Set_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Hashed_Sets_Visitors;

   -- Ada.Containers.Hashed_Maps
   generic
      with package Maps is new Ada.Containers.Hashed_Maps (<>);
      with procedure Visit_Key_Type (X : System.Address);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Hashed_Maps_Visitors is
      procedure AGC_Visit_Map_Private (X : System.Address);
      procedure AGC_Visit_Cursor_Private (X : System.Address)
         with Inline;

      package Map_Iterator_Interfaces_Forward_Iterator_Visitors is
         procedure AGC_Visit_Forward_Iterator_Classwide
           (X : System.Address) is null;
      end Map_Iterator_Interfaces_Forward_Iterator_Visitors;
   end Ada_Containers_Hashed_Maps_Visitors;

   --  Ada.Containers.Vectors
   generic
      with package Vectors is new Ada.Containers.Vectors (<>);
      with procedure Visit_Index_Type (X : System.Address);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Vectors_Visitors;

   --  Ada.Strings.Unbounded
   package Ada_Strings_Unbounded_Visitors is
      procedure AGC_Visit_Unbounded_String_Private
        (X : System.Address) is null;
   end Ada_Strings_Unbounded_Visitors;

   --  Ada.Strings.Wide_Unbounded
   package Ada_Strings_Wide_Unbounded_Visitors is
      procedure AGC_Visit_Unbounded_Wide_String_Private
        (X : System.Address) is null;
   end Ada_Strings_Wide_Unbounded_Visitors;

   --  Ada.Strings.Wide_Wide_Unbounded
   package Ada_Strings_Wide_Wide_Unbounded_Visitors is
      procedure AGC_Visit_Unbounded_Wide_Wide_String_Private
        (X : System.Address) is null;
   end Ada_Strings_Wide_Wide_Unbounded_Visitors;

   --  Ada.Text_IO
   package Ada_Text_IO_Visitors is
      procedure AGC_Visit_File_Type_Private
        (X : System.Address) is null;
   end Ada_Text_IO_Visitors;
end AGC.Standard;
