with Ada.Unchecked_Conversion;

package body AGC.Standard is
   package body Ada_Containers_Doubly_Linked_Lists_Visitors is
      procedure AGC_Visit_List_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         type List_Access is access all Lists.List;
         for List_Access'Size use Standard'Address_Size;

         function Conv is new Ada.Unchecked_Conversion
           (System.Address, List_Access);

         List : List_Access := Conv (X);
      begin
         for X of List.all loop
            declare
               E : aliased Lists.Element_Type := X;
            begin
               AGC_Visit_Element_Type (E'Address);
            end;
         end loop;
      end AGC_Visit_List_Private;
   end Ada_Containers_Doubly_Linked_Lists_Visitors;

   package body Ada_Containers_Indefinite_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address) is
         pragma Suppress (All_Checks);

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
               AGC_Visit_Element_Type (E'Address);
            end;
         end loop;
      end AGC_Visit_Vector_Private;
   end Ada_Containers_Indefinite_Vectors_Visitors;

   package body Ada_Containers_Hashed_Sets_Visitors is
      procedure AGC_Visit_Set_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         type Set_Access is access all Sets.Set;
         for Set_Access'Size use Standard'Address_Size;

         function Conv is new Ada.Unchecked_Conversion
           (System.Address, Set_Access);

         Set : Set_Access := Conv (X);
      begin
         for X of Set.all loop
            declare
               E : aliased Sets.Element_Type := X;
            begin
               AGC_Visit_Element_Type (E'Address);
            end;
         end loop;
      end AGC_Visit_Set_Private;
   end Ada_Containers_Hashed_Sets_Visitors;

   package body Ada_Containers_Hashed_Maps_Visitors is
      procedure Visit_Cursor (C : Maps.Cursor) is
         K : aliased Maps.Key_Type := Maps.Key (C);
         V : aliased Maps.Element_Type := Maps.Element (C);
      begin
         AGC_Visit_Key_Type (K'Address);
         AGC_Visit_Element_Type (V'Address);
      end Visit_Cursor;

      procedure AGC_Visit_Map_Private (X : System.Address) is
         pragma Suppress (All_Checks);

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
         pragma Suppress (All_Checks);

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

   package body Ada_Containers_Vectors_Visitors is
      procedure AGC_Visit_Vector_Private (X : System.Address) is
         pragma Suppress (All_Checks);

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
               AGC_Visit_Element_Type (E'Address);
            end;
         end loop;
      end AGC_Visit_Vector_Private;
   end Ada_Containers_Vectors_Visitors;
end AGC.Standard;
