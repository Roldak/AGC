with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   AGC_Base_Root_Count : Natural := AGC.Root_Count;
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   type Tree;
   type Tree_Kind is (Node, Leaf);
   type Tree_Access is access Tree;
   type Tree (K : Tree_Kind) is record
      case K is
         when Node =>
            Value : Integer;
            Left  : Tree_Access;
            Right : Tree_Access;
         when Leaf =>
            null;
      end case;
   end record;
   procedure AGC_Visit_Tree (X : System.Address);
   procedure AGC_Visit_Tree_Access (X : System.Address);
   procedure AGC_Visit_Tree_Access_Implem is new AGC.Visit_Access_Type
     (Main.Tree, Tree_Access, False, Main.AGC_Visit_Tree);
   procedure AGC_Visit_Tree_Access (X : System.Address) renames
     AGC_Visit_Tree_Access_Implem;
   procedure AGC_Visit_Tree (X : System.Address) is
      pragma Suppress (Accessibility_Check);
      type Rec_Access is access all Tree;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : aliased Tree := Conv (X).all;
   begin
      case R.K is
         when Node =>
            declare
               C : aliased Main.Tree_Access := R.Left;
            begin
               Main.AGC_Visit_Tree_Access (C'Address);
            end;
            declare
               C : aliased Main.Tree_Access := R.Right;
            begin
               Main.AGC_Visit_Tree_Access (C'Address);
            end;
         when Leaf =>
            null;
      end case;
   end AGC_Visit_Tree;
   Empty_Tree : aliased Tree_Access := new Tree'(K => Leaf);
   function Insert (T : Tree_Access; V : Integer) return Tree_Access is
      AGC_Base_Root_Count : Natural := AGC.Root_Count;
   begin
      case T.K is
         when Node =>
            if T.Value > V then
               declare
                  AGC_Root_Count : Natural                  := AGC.Root_Count;
                  AGC_Temp_0 : aliased Main.Tree_Access := Insert (T.Left, V);
               begin
                  AGC.Push_Root
                    (AGC_Temp_0'Address, Main.AGC_Visit_Tree_Access'Address);
                  return
                    AGC_Ret : Tree_Access :=
                      new Tree'(Node, T.Value, AGC_Temp_0, T.Right) do
                     AGC.Pop_Roots (AGC_Base_Root_Count);
                  end return;
               end;
            elsif T.Value < V then
               declare
                  AGC_Root_Count : Natural                  := AGC.Root_Count;
                  AGC_Temp_0 : aliased Main.Tree_Access := Insert (T.Right, V);
               begin
                  AGC.Push_Root
                    (AGC_Temp_0'Address, Main.AGC_Visit_Tree_Access'Address);
                  return
                    AGC_Ret : Tree_Access :=
                      new Tree'(Node, T.Value, T.Left, AGC_Temp_0) do
                     AGC.Pop_Roots (AGC_Base_Root_Count);
                  end return;
               end;
            else
               return AGC_Ret : Tree_Access := T do
                  AGC.Pop_Roots (AGC_Base_Root_Count);
               end return;
            end if;
         when Leaf =>
            return
              AGC_Ret : Tree_Access :=
                new Tree'(Node, V, Empty_Tree, Empty_Tree) do
               AGC.Pop_Roots (AGC_Base_Root_Count);
            end return;
      end case;
   end Insert;
   function To_String (T : Tree_Access) return String is
      AGC_Base_Root_Count : Natural := AGC.Root_Count;
   begin
      case T.K is
         when Node =>
            return
              AGC_Ret : String :=
                "(" & To_String (T.Left) & T.Value'Image & " " &
                To_String (T.Right) & ")"
            do
               AGC.Pop_Roots (AGC_Base_Root_Count);
            end return;
         when Leaf =>
            return AGC_Ret : String := "<>" do
               AGC.Pop_Roots (AGC_Base_Root_Count);
            end return;
      end case;
   end To_String;
   procedure Bench (Rng : Integer) is
      AGC_Base_Root_Count : Natural             := AGC.Root_Count;
      T                   : aliased Tree_Access := Insert (Empty_Tree, 0);
   begin
      AGC.Push_Root (T'Address, Main.AGC_Visit_Tree_Access'Address);
      for I in Integer range 1 .. Rng loop
         declare
            AGC_Root_Count : Natural                  := AGC.Root_Count;
            AGC_Temp_0     : aliased Main.Tree_Access := Insert (T, I);
         begin
            AGC.Push_Root
              (AGC_Temp_0'Address, Main.AGC_Visit_Tree_Access'Address);
            T := Insert (AGC_Temp_0, -I);
            AGC.Pop_Roots (AGC_Root_Count);
         end;
      end loop;
      AGC.Pop_Roots (AGC_Base_Root_Count);
   end Bench;
begin
   AGC.Push_Root (Empty_Tree'Address, Main.AGC_Visit_Tree_Access'Address);
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "Expected one argument";
   end if;
   Bench (Integer'Value (Ada.Command_Line.Argument (1)));
   AGC.Pop_Roots (AGC_Base_Root_Count);
   AGC.Collect;
   AGC.Print_Stats;
end Main;
