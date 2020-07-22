with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with System; use System;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

package body GC is
   type Address_Access is access all Address;

   function As_Address_Access is new Ada.Unchecked_Conversion
     (Address, Address_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Address, Address_Access);

   procedure Collect (Value : in Address) is
      Var : Address_Access := As_Address_Access (Value);
   begin
      Free (Var);
   end Collect;

   type Alloc_State is (Unknown, Reachable, Temporary);

   type Temporary_Site is record
      Site_Id : Natural;
      Value   : Address;
   end record;

   package Address_Maps is new Ada.Containers.Ordered_Maps (Address, Alloc_State);
   package Address_Vectors is new Ada.Containers.Vectors (Positive, Address);
   package Temp_Site_Vectors is new Ada.Containers.Vectors (Positive, Temporary_Site);

   Alloc_Set : Address_Maps.Map;
   Reach_Set : Address_Vectors.Vector;
   Temps_Set : Temp_Site_Vectors.Vector;

   procedure Push_Reachable (X : Address) is
   begin
      Address_Vectors.Append (Reach_Set, X);
   end Push_Reachable;

   procedure Pop_Reachable (Count : Ada.Containers.Count_Type) is
      use type Ada.Containers.Count_Type;
   begin
      Address_Vectors.Set_Length
        (Reach_Set, Address_Vectors.Length (Reach_Set) - Count);
   end Pop_Reachable;

   function Register (X : access Integer) return access Integer
   is
		pragma Suppress (Accessibility_Check);

      Addr : Address := X.all'Address;
   begin
      Collect;
      Put_Line ("Adding " & Address_Image (Addr));
      Address_Maps.Insert (Alloc_Set, Addr, Unknown);
      return X;
   end Register;

   function Temp
     (Site_Id : Natural; X : access Integer) return access Integer
   is
		pragma Suppress (Accessibility_Check);

      procedure Mark_Temp (A : Address; V : in out Alloc_State) is
      begin
         if V = Unknown then
            V := Temporary;
         end if;
      end Mark_Temp;

      Addr : Address := X.all'Address;

		Alloc_Elem : Address_Maps.Cursor :=
			Address_Maps.Find (Alloc_Set, Addr);
   begin
      Temp_Site_Vectors.Append (Temps_Set, (Site_Id, Addr));
		Address_Maps.Update_Element
 		  (Alloc_Set, Alloc_Elem, Mark_Temp'Access);
      return X;
   end Temp;

   procedure Untemp (Site_Id : Natural) is
      use type Temp_Site_Vectors.Cursor;

      procedure Mark_Unknown (A : Address; V : in out Alloc_State) is
      begin
         if V = Temporary then
            V := Unknown;
         end if;
      end Mark_Unknown;

		Cursor  : Temp_Site_Vectors.Cursor := Temps_Set.Last;
		Found   : Boolean := False;
   begin
		while Cursor /= Temp_Site_Vectors.No_Element loop
         declare
            Temp_Elem : Temporary_Site :=
               Temp_Site_Vectors.Element (Cursor);

            Matches_Site : Boolean := Temp_Elem.Site_Id = Site_Id;

            Alloc_Elem : Address_Maps.Cursor :=
               Address_Maps.Find (Alloc_Set, Temp_Elem.Value);
         begin
            if Matches_Site then
               Found := True;
            end if;
            exit when Found and not Matches_Site;

            Address_Maps.Update_Element
              (Alloc_Set, Alloc_Elem, Mark_Unknown'Access);
			   Cursor := Temp_Site_Vectors.Previous (Cursor);
         end;
		end loop;

      if Cursor = Temp_Site_Vectors.No_Element then
         Temps_Set.Set_Length (0);
      else
         Temps_Set.Set_Length
           (Ada.Containers.Count_Type
              (Temp_Site_Vectors.To_Index (Cursor) + 1));
      end if;
   end Untemp;

   procedure Collect is
      procedure Mark_Reached (A : Address; V : in out Alloc_State) is
      begin
         V := Reachable;
      end Mark_Reached;

      procedure Mark_Unknown (A : Address; V : in out Alloc_State) is
      begin
         if V = Reachable then
            V := Unknown;
         end if;
      end Mark_Unknown;

      use type Address_Maps.Cursor;
   begin
      for Addr of Reach_Set loop
         declare
            Ref  : Address := As_Address_Access (Addr).all;
            Elem : Address_Maps.Cursor := Address_Maps.Find
              (Alloc_Set, Ref);
         begin
            if Elem /= Address_Maps.No_Element then
               Address_Maps.Update_Element
                 (Alloc_Set, Elem, Mark_Reached'Access);
            end if;
         end;
      end loop;

      declare
         Elem  : Address_Maps.Cursor := Address_Maps.First (Alloc_Set);
         Next  : Address_Maps.Cursor;
         State : Alloc_State;
         Key   : Address;
      begin
         while Elem /= Address_Maps.No_Element loop
            State := Address_Maps.Element (Elem);
            Next  := Address_Maps.Next (Elem);
            Key   := Address_Maps.Key (Elem);

            if State /= Unknown then
               Put_Line ("Keeping " & Address_Image (Key));
               Address_Maps.Update_Element
                 (Alloc_Set, Elem, Mark_Unknown'Access);
            else
               Put_Line ("Collecting " & Address_Image (Key));
               Collect (Key);
               Address_Maps.Delete (Alloc_Set, Elem);
            end if;

            Elem := Next;
         end loop;
      end;
   end Collect;

   procedure Print_Stats is
   begin
      Put_Line ("Still alive : " & Address_Maps.Length (Alloc_Set)'Image);
   end Print_Stats;
end GC;
