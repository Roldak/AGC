package Analysis.Dataflow is
   package LAL renames Libadalang.Analysis;

   generic
      type T is private;
      with function "or"  (X, Y : T) return T;
      with function "and" (X, Y : T) return T;
      with function "<="  (X, Y : T) return Boolean;
      with function Image (X : T) return String;
   package Lattice is
      function "=" (X, Y : T) return Boolean;
      function "<" (X, Y : T) return Boolean;
   end Lattice;

   generic
      with package Sets is new Ada.Containers.Hashed_Sets (<>);
      with function Element_Image (X : Sets.Element_Type) return String;
   package Finite_Sets is
      function Image (X : Sets.Set) return String;

      package Lattice is new Dataflow.Lattice
        (Sets.Set, Sets.Union, Sets.Intersection, Sets.Is_Subset, Image);
   end Finite_Sets;

   generic
      with package States is new Lattice (<>);

      Entry_State : States.T;

      with procedure Visit_Assign
        (State : in out States.T;
         Dest  : LAL.Name;
         Val   : LAL.Expr) is null;

      with procedure Visit_Ignore
        (State : in out States.T;
         Expr  : LAL.Expr) is null;

      with procedure Visit_Return
        (State : in out States.T;
         Expr  : LAL.Expr) is null;
   package Problem is
      type Solution is tagged private;

      function Fixpoint (Subp  : LAL.Base_Subp_Body) return Solution;

      function Query_At
        (S : Solution; Node : LAL.Ada_Node) return States.T;

      function Query_After
        (S : Solution; Node : LAL.Ada_Node) return States.T;

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
