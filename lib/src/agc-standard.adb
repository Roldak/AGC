with Ada.Unchecked_Conversion;

package body AGC.Standard is
   package body Ada_Containers_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address) is
         pragma Suppress (Accessibility_Check);

         type Vector_Access is access all Vectors.Vector;
         for Vector_Access'Size use Standard'Address_Size;

         function Conv is new Ada.Unchecked_Conversion
           (System.Address, Vector_Access);

         Vec : Vector_Access := Conv (X);
      begin
         for X of Vec.all loop
            declare
               E : aliased Vectors.Element_Type := X;
            begin
               Visit_Element (E'Address);
            end;
         end loop;
      end AGC_Visit_Vector_Private;
   end Ada_Containers_Vectors_Visitors;
end AGC.Standard;
