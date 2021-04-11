with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
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

   procedure AGC_Visit_Tree (X : System.Address) with
      Inline;
   for Tree_Access'Storage_Pool use AGC.Storage.Get.Pool;
   procedure AGC_Visit_Tree_Access (X : System.Address) with
      Inline;
   package AGC_Tree_Access_Ops_Implem is new AGC.Access_Type_Operations
     (Main.Tree, Tree_Access, False, Main.AGC_Visit_Tree);
   procedure AGC_Visit_Tree_Access (X : System.Address) renames
     AGC_Tree_Access_Ops_Implem.Mark_And_Visit_Access_Type;
   function AGC_Register_Tree_Access (X : Tree_Access) return Tree_Access with
      Inline;
   function AGC_Register_Tree_Access
     (X : Tree_Access) return Tree_Access renames
     AGC_Tree_Access_Ops_Implem.Register;
   procedure AGC_Visit_Tree (X : System.Address) is
      pragma Suppress (All_Checks);
      type Rec_Access is access Tree with
         Storage_Size => 0;
      for Rec_Access'Size use Standard'Address_Size;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Rec_Access);
      R : Tree renames Conv (X).all;
   begin
      case R.K is
         when Node =>
            Main.AGC_Visit_Tree_Access (R.Left'Address);
            Main.AGC_Visit_Tree_Access (R.Right'Address);
         when Leaf =>
            null;
      end case;
   end AGC_Visit_Tree;
   Empty_Tree : aliased Tree_Access :=
     Main.AGC_Register_Tree_Access (Main.Tree_Access'(new Tree'(K => Leaf)));

   AGC_Dummy_0 : constant AGC.Empty_Type :=
     AGC.Push_Root (Empty_Tree'Address, Main.AGC_Visit_Tree_Access'Address);
   function Insert (T : Tree_Access; V : Integer) return Tree_Access is
      AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
   begin
      case T.K is
         when Node =>
            if T.Value > V then
               declare
                  AGC_Temp_0  : aliased Main.Tree_Access := Insert (T.Left, V);
                  AGC_Dummy_0 : constant AGC.Empty_Type  :=
                    AGC.Push_Root
                      (AGC_Temp_0'Address, Main.AGC_Visit_Tree_Access'Address);
               begin
                  return
                    AGC_Ret : Tree_Access :=
                      Main.AGC_Register_Tree_Access
                        (Main.Tree_Access'
                           (new Tree'(Node, T.Value, AGC_Temp_0, T.Right)))
                  do
                     AGC.Pop_Roots (AGC_Base_Root_Count);
                  end return;
               end;
            elsif T.Value < V then
               declare
                  AGC_Temp_0 : aliased Main.Tree_Access := Insert (T.Right, V);
                  AGC_Dummy_1 : constant AGC.Empty_Type  :=
                    AGC.Push_Root
                      (AGC_Temp_0'Address, Main.AGC_Visit_Tree_Access'Address);
               begin
                  return
                    AGC_Ret : Tree_Access :=
                      Main.AGC_Register_Tree_Access
                        (Main.Tree_Access'
                           (new Tree'(Node, T.Value, T.Left, AGC_Temp_0)))
                  do
                     AGC.Pop_Roots (AGC_Base_Root_Count);
                  end return;
               end;
            else
               AGC.Pop_Roots (AGC_Base_Root_Count);
               return T;
            end if;
         when Leaf =>
            return
              AGC_Ret : Tree_Access :=
                Main.AGC_Register_Tree_Access
                  (Main.Tree_Access'
                     (new Tree'(Node, V, Empty_Tree, Empty_Tree)))
            do
               AGC.Pop_Roots (AGC_Base_Root_Count);
            end return;
      end case;
   end Insert;

   function To_String (T : Tree_Access) return String is
   begin
      case T.K is
         when Node =>
            return
              AGC_Ret : String :=
                "(" & To_String (T.Left) & T.Value'Image & " " &
                To_String (T.Right) & ")"
            do
               null;
            end return;
         when Leaf =>
            return AGC_Ret : String := "<>" do
               null;
            end return;
      end case;
   end To_String;

   procedure Bench (Rng : Integer) is
      AGC_Base_Root_Count : constant Natural        := AGC.Root_Count;
      T                   : aliased Tree_Access     := Insert (Empty_Tree, 0);
      AGC_Dummy_0         : constant AGC.Empty_Type :=
        AGC.Push_Root (T'Address, Main.AGC_Visit_Tree_Access'Address);
   begin
      for I in Integer range 1 .. Rng loop
         declare
            AGC_Root_Count : constant Natural         := AGC.Root_Count;
            AGC_Temp_0     : aliased Main.Tree_Access := Insert (T, I);
            AGC_Dummy_1    : constant AGC.Empty_Type  :=
              AGC.Push_Root
                (AGC_Temp_0'Address, Main.AGC_Visit_Tree_Access'Address);
         begin
            T := Insert (AGC_Temp_0, -I);
            AGC.Pop_Roots (AGC_Root_Count);
         end;
      end loop;
      AGC.Pop_Roots (AGC_Base_Root_Count);
   end Bench;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "Expected one argument";
   end if;
   Bench (Integer'Value (Ada.Command_Line.Argument (1)));
   AGC.Pop_Roots (AGC_Base_Root_Count);
end Main;
