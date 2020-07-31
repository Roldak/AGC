with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Tree;
   type Tree_Kind is (Node, Leaf);
   type Tree_Access is access all Tree;
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

   Empty_Tree : Tree_Access := new Tree'(K => Leaf);

   function Insert (T : Tree_Access; V : Integer) return Tree_Access is
   begin
      case T.K is
         when Node =>
            if T.Value > V then
               return new Tree'(Node, T.Value, Insert (T.Left, V), T.Right);
            elsif T.Value < V then
               return new Tree'(Node, T.Value, T.Left, Insert (T.Right, V));
            else
               return T;
            end if;
         when Leaf =>
            return new Tree'(Node, V, Empty_Tree, Empty_Tree);
      end case;
   end Insert;

   function To_String (T : Tree_Access) return String is
   begin
      case T.K is
         when Node =>
            return "(" & To_String (T.Left)
                       & T.Value'Image
                       & " " & To_String (T.Right) & ")";
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
