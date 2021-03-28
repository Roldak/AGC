with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with System;

package AGC.Standard is
   --  Ada.Containers.Vectors
   generic
      with package Vectors is new Ada.Containers.Vectors (<>);
      with procedure Visit_Index_Type (X : System.Address);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Vectors_Visitors;

   --  Ada.Containers.Indefinite_Vectors
   generic
      with package Vectors is new Ada.Containers.Indefinite_Vectors (<>);
      with procedure Visit_Index_Type (X : System.Address);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Indefinite_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Indefinite_Vectors_Visitors;

   -- Ada.Containers.Hashed_Maps
   generic
      with package Maps is new Ada.Containers.Hashed_Maps (<>);
      with procedure Visit_Key_Type (X : System.Address);
      with procedure Visit_Element_Type (X : System.Address);
   package Ada_Containers_Hashed_Maps_Visitors is
      procedure AGC_Visit_Map_Private (X : System.Address);
      procedure AGC_Visit_Cursor_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Hashed_Maps_Visitors;

   --  Ada.Strings.Unbounded
   package Ada_Strings_Unbounded_Visitors is
      procedure AGC_Visit_Unbounded_String_Private (X : System.Address)
         renames AGC.No_Op;
   end Ada_Strings_Unbounded_Visitors;
end AGC.Standard;
