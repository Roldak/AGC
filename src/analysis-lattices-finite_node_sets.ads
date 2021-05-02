with Libadalang.Analysis;

package Analysis.Lattices.Finite_Node_Sets is
   package LAL renames Libadalang.Analysis;

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   function Node_Image (X : LAL.Ada_Node) return String is
     (Langkit_Support.Text.Image (X.Text));

   package Sets is new Finite_Sets
     (Node_Sets, Node_Image);

   package Lattice renames Sets.Lattice;
end Analysis.Lattices.Finite_Node_Sets;
