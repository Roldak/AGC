with Analysis.Lattices;

package Analysis.Dataflow is
   use Lattices;

   package LAL renames Libadalang.Analysis;

   type Confluence_Type is (May, Must);
   type Direction_Type is (Forward, Backwards);

   generic
      with package States is new Lattice (<>);

      Confluence : Confluence_Type;
      Flow       : Direction_Type;

      with procedure Visit_Parameter
        (State : in out States.T;
         Ctx   : LAL.Base_Subp_Body;
         Param : LAL.Param_Spec) is null;

      with procedure Visit_Assign
        (State : in out States.T;
         Ctx   : LAL.Base_Subp_Body;
         Dest  : LAL.Name;
         Val   : LAL.Expr) is null;

      with procedure Visit_Ignore
        (State : in out States.T;
         Ctx   : LAL.Base_Subp_Body;
         Expr  : LAL.Expr) is null;

      with procedure Visit_Return
        (State : in out States.T;
         Ctx   : LAL.Base_Subp_Body;
         Expr  : LAL.Expr) is null;

      Entry_State : States.T;
   package Problem is
      type Solution is tagged private;

      function Fixpoint (Subp  : LAL.Base_Subp_Body) return Solution;

      function Query_At
        (S : Solution; Node : LAL.Ada_Node) return States.T;

      procedure Query_After
        (S       : Solution;
         Node    : LAL.Ada_Node'Class;
         Process : access procedure (N : LAL.Ada_Node; V : States.T));

      procedure Query_Before
        (S       : Solution;
         Node    : LAL.Ada_Node'Class;
         Process : access procedure (N : LAL.Ada_Node; V : States.T));

      procedure Iterate
        (S : Solution;
         F : access procedure (N : LAL.Ada_Node; V : States.T));

      procedure Dump (S : Solution);

      procedure Output_Graph (S : Solution; Path : String);

   private
      package State_Maps is new Ada.Containers.Hashed_Maps
        (LAL.Ada_Node,
         States.T,
         LAL.Hash,
         LAL."=",
         States."=");

      type Solution is tagged record
         States : State_Maps.Map;
      end record;
   end Problem;
end Analysis.Dataflow;
