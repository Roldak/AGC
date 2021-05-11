with Analysis.Dataflow;
with Analysis.Lattices.Finite_Node_Sets;

package Analysis.Ownership is
   package Node_Sets renames Analysis.Lattices.Finite_Node_Sets.Node_Sets;

   use Ada.Strings.Wide_Wide_Unbounded;

   package Id_Sets is new Ada.Containers.Hashed_Sets
     (Unbounded_Text_Type, Wide_Wide_Hash, "=");

   use Analysis.Lattices;

   procedure Include_Parameter
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Param : LAL.Param_Spec);

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
     (States          => Finite_Node_Sets.Lattice,
      Confluence      => Dataflow.Must,
      Flow            => Dataflow.Forward,
      Visit_Parameter => Include_Parameter,
      Visit_Assign    => Handle_Assignment,
      Visit_Ignore    => Remove_Possibly_Aliased,
      Entry_State     => Node_Sets.Empty_Set);

   function Analyze (X : LAL.Body_Node) return Problem.Solution;
   function Default (X : LAL.Body_Node) return Problem.Solution;

   type Universal_Solution is record
      Final_Owners  : Id_Sets.Set;
      Returns_Owner : Boolean;
   end record;

   function To_Universal
     (X : Problem.Solution) return Universal_Solution;

   function Is_Owner
     (Result : Universal_Solution;
      Param  : LAL.Defining_Name) return Boolean;

   package Share is new Shared_Analysis
     (Problem.Solution,
      Analyze,
      Default,
      Universal_Solution,
      To_Universal,
      Problem."=");
end Analysis.Ownership;
