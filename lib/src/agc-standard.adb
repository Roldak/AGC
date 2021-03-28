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
               Visit_Element_Type (E'Address);
            end;
         end loop;
      end AGC_Visit_Vector_Private;
   end Ada_Containers_Vectors_Visitors;

   package body Ada_Containers_Hashed_Maps_Visitors is
      procedure Visit_Cursor (C : Maps.Cursor) is
         K : aliased Maps.Key_Type := Maps.Key (C);
         V : aliased Maps.Element_Type := Maps.Element (C);
      begin
         Visit_Key_Type (K'Address);
         Visit_Element_Type (V'Address);
      end Visit_Cursor;

      procedure AGC_Visit_Map_Private (X : System.Address) is
         pragma Suppress (Accessibility_Check);

         type Map_Access is access all Maps.Map;
         for Map_Access'Size use Standard'Address_Size;

         function Conv is new Ada.Unchecked_Conversion
           (System.Address, Map_Access);

         Map : Map_Access := Conv (X);
         C   : Maps.Cursor := Maps.First (Map.all);
      begin
         while Maps.Has_Element (C) loop
            Visit_Cursor (C);
            Maps.Next (C);
         end loop;
      end AGC_Visit_Map_Private;

      procedure AGC_Visit_Cursor_Private (X : System.Address) is
         pragma Suppress (Accessibility_Check);

         type Cursor_Access is access all Maps.Cursor;
         for Cursor_Access'Size use Standard'Address_Size;

         function Conv is new Ada.Unchecked_Conversion
           (System.Address, Cursor_Access);

         Cursor : Cursor_Access := Conv (X);
      begin
         if Maps.Has_Element (Cursor.all) then
            Visit_Cursor (Cursor.all);
         end if;
      end AGC_Visit_Cursor_Private;
   end Ada_Containers_Hashed_Maps_Visitors;
end AGC.Standard;
