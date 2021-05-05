with Analysis.Dataflow;
with Analysis.Lattices.Finite_Node_Sets;

package Analysis.Ownership is
   package Node_Sets renames Analysis.Lattices.Finite_Node_Sets.Node_Sets;

   use Analysis.Lattices;

   procedure Remove_Possibly_Aliased
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Expr  : LAL.Expr);

   procedure Handle_Assignment
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Dest  : LAL.Name;
      Val   : LAL.Expr);

   package Problem is new Dataflow.Problem
     (States       => Finite_Node_Sets.Lattice,
      Confluence   => Dataflow.Must,
      Flow         => Dataflow.Forward,
      Visit_Assign => Handle_Assignment,
      Visit_Ignore => Remove_Possibly_Aliased,
      Entry_State  => Node_Sets.Empty_Set);

   function Analyze (X : LAL.Body_Node) return Problem.Solution;
   function Default (X : LAL.Body_Node) return Problem.Solution;

   type Universal_Solution is null record;

   function To_Universal
     (X : Problem.Solution) return Universal_Solution
   is
     (null record);

   package Share is new Shared_Analysis
     (Problem.Solution,
      Analyze,
      Default,
      Universal_Solution,
      To_Universal,
      Problem."=");
end Analysis.Ownership;
