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

   type Alloc_State is (Unknown, Reachable, Temporary);

   package Address_Maps is new Ada.Containers.Ordered_Maps (Address, Alloc_State);
   package Address_Vectors is new Ada.Containers.Vectors (Positive, Address);
   package Alloc_Site_Maps is new Ada.Containers.Ordered_Maps (Natural, Address);

   Alloc_Set : Address_Maps.Map;
   Reach_Set : Address_Vectors.Vector;
   Temps_Set  : Alloc_Site_Maps.Map;

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

   function Register
     (Site_Id : Natural; X : access Integer) return access Integer
   is
		pragma Suppress (Accessibility_Check);

      Addr : Address := X.all'Address;
   begin
      Collect;
      Put_Line ("Adding " & Address_Image (Addr));
      Address_Maps.Insert (Alloc_Set, Addr, Temporary);
      Alloc_Site_Maps.Insert (Temps_Set, Site_Id, Addr);
      return X;
   end Register;

   procedure Untemp (Site_Id : Natural) is
      procedure Mark_Unknown (A : Address; V : in out Alloc_State) is
      begin
         if V = Temporary then
            V := Unknown;
         end if;
      end Mark_Unknown;

      Site_Elem : Alloc_Site_Maps.Cursor :=
         Alloc_Site_Maps.Find (Temps_Set, Site_Id);

      Addr : Address := Alloc_Site_Maps.Element (Site_Elem);

      Alloc_Elem : Address_Maps.Cursor :=
         Address_Maps.Find (Alloc_Set, Addr);
   begin
      Alloc_Site_Maps.Delete (Temps_Set, Site_Elem);

      Address_Maps.Update_Element
        (Alloc_Set, Alloc_Elem, Mark_Unknown'Access);
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
      begin
         while Elem /= Address_Maps.No_Element loop
            State := Address_Maps.Element (Elem);
            Next  := Address_Maps.Next (Elem);

            if State /= Unknown then
               Put_Line ("Keeping " & Address_Image
                           (Address_Maps.Key (Elem)));
               Address_Maps.Update_Element
                 (Alloc_Set, Elem, Mark_Unknown'Access);
            else
               Put_Line ("Collecting " & Address_Image
                           (Address_Maps.Key (Elem)));
               Address_Maps.Delete (Alloc_Set, Elem);
            end if;

            Elem := Next;
         end loop;
      end;
   end Collect;
end GC;
