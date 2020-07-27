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
   type Address_Visitor is access procedure (X : Address);

   function As_Address_Access is new Ada.Unchecked_Conversion
     (Address, Address_Access);

   function As_Address_Visitor is new Ada.Unchecked_Conversion
     (Address, Address_Visitor);

   procedure Free is new Ada.Unchecked_Deallocation
     (Address, Address_Access);

   procedure Collect (Value : in Address) is
      Var : Address_Access := As_Address_Access (Value);
   begin
      Free (Var);
   end Collect;

   type Alloc_State is (Unknown, Reachable);
   type Root is record
      Addr    : Address;
      Visitor : Address;
   end record;

   package Address_Maps is new Ada.Containers.Ordered_Maps (Address, Alloc_State);
   package Root_Vectors is new Ada.Containers.Vectors (Positive, Root);

   Alloc_Set : Address_Maps.Map;
   Reach_Set : Root_Vectors.Vector;

   function Root_Count return Natural is (Natural (Reach_Set.Length));

   procedure Push_Root (X, Visitor : Address) is
   begin
      Reach_Set.Append ((X, Visitor));
   end Push_Root;

   procedure Pop_Roots (X : Natural) is
   begin
      Reach_Set.Set_Length (Ada.Containers.Count_Type (X));
   end Pop_Roots;

   function Register (X : access T) return access T
   is
      Addr : Address := X.all'Address;
   begin
      Collect;
      Put_Line ("Adding " & Address_Image (Addr));
      Address_Maps.Insert (Alloc_Set, Addr, Unknown);
      return X;
   end Register;

   procedure Mark (Addr : Address) is
      use type Address_Maps.Cursor;

      Cursor : Address_Maps.Cursor :=
         Address_Maps.Find (Alloc_Set, Addr);
   begin
      if Cursor /= Address_Maps.No_Element then
         Address_Maps.Replace_Element
           (Alloc_Set, Cursor, Reachable);
      end if;
   end Mark;

   procedure No_Op (X : T) is null;

   procedure Visit_Access_Type (X : T_Access) is
   begin
      if X /= null then
         Mark (X.all'Address);
         Visit_Element (X.all);
      end if;
   end Visit_Access_Type;

   procedure Collect is
      use type Address_Maps.Cursor;
   begin
      for Root of Reach_Set loop
         declare
            Ref  : Address := As_Address_Access (Root.Addr).all;
         begin
            As_Address_Visitor (Root.Visitor) (Ref);
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
               Address_Maps.Replace_Element (Alloc_Set, Elem, Unknown);
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
