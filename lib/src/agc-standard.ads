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

   --  Ada.Strings.Unbounded
   package Ada_Strings_Unbounded_Visitors is
      procedure AGC_Visit_Unbounded_String_Private (X : System.Address)
         renames AGC.No_Op;
   end Ada_Strings_Unbounded_Visitors;
end AGC.Standard;
