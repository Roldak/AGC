with Ada.Unchecked_Conversion;

package body AGC.Standard is
   package body Ada_Containers_Doubly_Linked_Lists_Visitors is
      type List_Access is access Lists.List
         with Storage_Size => 0;
      for List_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (System.Address, List_Access);

      procedure AGC_Visit_List_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         List : List_Access := Conv (X);
      begin
         for X of List.all loop
            AGC_Visit_Element_Type (X'Address);
         end loop;
      end AGC_Visit_List_Private;
   end Ada_Containers_Doubly_Linked_Lists_Visitors;

   package body Ada_Containers_Indefinite_Vectors_Visitors is
      type Vector_Access is access Vectors.Vector
         with Storage_Size => 0;
      for Vector_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Vector_Access);

      procedure AGC_Visit_Vector_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         Vec : Vector_Access := Conv (X);
      begin
         for X of Vec.all loop
            AGC_Visit_Element_Type (X'Address);
         end loop;
      end AGC_Visit_Vector_Private;
   end Ada_Containers_Indefinite_Vectors_Visitors;

   package body Ada_Containers_Hashed_Sets_Visitors is
      type Set_Access is access Sets.Set
         with Storage_Size => 0;
      for Set_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Set_Access);

      procedure AGC_Visit_Set_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         Set : Set_Access := Conv (X);
      begin
         for X of Set.all loop
            AGC_Visit_Element_Type (X'Address);
         end loop;
      end AGC_Visit_Set_Private;
   end Ada_Containers_Hashed_Sets_Visitors;

   package body Ada_Containers_Hashed_Maps_Visitors is
      type Map_Access is access Maps.Map
         with Storage_Size => 0;
      for Map_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Map_Access);

      procedure Visit_Cursor (C : Maps.Cursor) is
      begin
         AGC_Visit_Key_Type (Maps.Key (C)'Address);
         AGC_Visit_Element_Type (Maps.Element (C)'Address);
      end Visit_Cursor;

      procedure AGC_Visit_Map_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         Map : Map_Access := Conv (X);
         C   : Maps.Cursor := Maps.First (Map.all);
      begin
         while Maps.Has_Element (C) loop
            Visit_Cursor (C);
            Maps.Next (C);
         end loop;
      end AGC_Visit_Map_Private;

      type Cursor_Access is access Maps.Cursor
         with Storage_Size => 0;
      for Cursor_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Cursor_Access);

      procedure AGC_Visit_Cursor_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         Cursor : Cursor_Access := Conv (X);
      begin
         if Maps.Has_Element (Cursor.all) then
            Visit_Cursor (Cursor.all);
         end if;
      end AGC_Visit_Cursor_Private;
   end Ada_Containers_Hashed_Maps_Visitors;

   package body Ada_Containers_Vectors_Visitors is
      type Vector_Access is access Vectors.Vector
         with Storage_Size => 0;
      for Vector_Access'Size use Standard'Address_Size;

      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Vector_Access);

      procedure AGC_Visit_Vector_Private (X : System.Address) is
         pragma Suppress (All_Checks);

         Vec : Vector_Access := Conv (X);
      begin
         for X of Vec.all loop
            AGC_Visit_Element_Type (X'Address);
         end loop;
      end AGC_Visit_Vector_Private;
   end Ada_Containers_Vectors_Visitors;
end AGC.Standard;
