with Ada.Containers.Vectors;

with System;

package AGC.Standard is
   generic
      with package Vectors is new Ada.Containers.Vectors (<>);
      with procedure Visit_Element (X : System.Address);
   package Ada_Containers_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address)
         with Inline;
   end Ada_Containers_Vectors_Visitors;
end AGC.Standard;
