with Ada.Command_Line;
with Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure Main is
   type Tree;
   type Tree_Kind is (Node, Leaf);
   type Tree_Access_Internal is access all Tree;
   type Tree_Access is new Ada.Finalization.Controlled with record
      Element : Tree_Access_Internal;
   end record;

   overriding procedure Initialize (T : in out Tree_Access);
   overriding procedure Adjust (T : in out Tree_Access);
   overriding procedure Finalize (T : in out Tree_Access);

   type Tree (K : Tree_Kind) is record
      Ref_Count : Natural := 0;
      case K is
         when Node =>
            Value : Integer;
            Left  : Tree_Access;
            Right : Tree_Access;
         when Leaf =>
            null;
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Tree, Tree_Access_Internal);

   overriding procedure Initialize (T : in out Tree_Access) is
   begin
      if T.Element /= null then
         T.Element.Ref_Count := T.Element.Ref_Count + 1;
      end if;
   end Initialize;

   overriding procedure Adjust (T : in out Tree_Access) is
   begin
      if T.Element /= null then
         T.Element.Ref_Count := T.Element.Ref_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (T : in out Tree_Access) is
   begin
      if T.Element /= null then
         T.Element.Ref_Count := T.Element.Ref_Count - 1;
         if T.Element.Ref_Count = 0 then
            Free (T.Element);
         end if;
      end if;
   end Finalize;

   function Create_Node
     (Value : Integer; Lhs, Rhs : Tree_Access) return Tree_Access
   is
   begin
      return (Ada.Finalization.Controlled
              with Element => new Tree'(Node, 1, Value, Lhs, Rhs));
   end Create_Node;

   function Create_Leaf return Tree_Access is
   begin
      return (Ada.Finalization.Controlled
              with Element => new Tree'(Leaf, 1));
   end Create_Leaf;

   Empty_Tree : Tree_Access := Create_Leaf;

   function Insert (T : Tree_Access; V : Integer) return Tree_Access is
      E : Tree_Access_Internal := T.Element;
   begin
      case E.K is
         when Node =>
            if E.Value > V then
               return Create_Node (E.Value, Insert (E.Left, V), E.Right);
            elsif E.Value < V then
               return Create_Node (E.Value, E.Left, Insert (E.Right, V));
            else
               return T;
            end if;
         when Leaf =>
            return Create_Node (V, Empty_Tree, Empty_Tree);
      end case;
   end Insert;

   function To_String (T : Tree_Access) return String is
      E : Tree_Access_Internal := T.Element;
   begin
      case E.K is
         when Node =>
            return "(" & To_String (E.Left)
                       & E.Value'Image
                       & " " & To_String (E.Right) & ")";
         when Leaf =>
            return "<>";
      end case;
   end To_String;

   procedure Bench (Rng : Integer) is
      T : Tree_Access := Insert (Empty_Tree, 0);
   begin
      for I in Integer range 1 .. Rng loop
         T := Insert (Insert (T, I), -I);
      end loop;
   end Bench;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "Expected one argument";
   end if;
   Bench (Integer'Value (Ada.Command_Line.Argument (1)));
end Main;
