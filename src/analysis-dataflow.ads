package Analysis.Dataflow is
   package LAL renames Libadalang.Analysis;

   generic
      type T is private;
      with function Hash (X : T) return Ada.Containers.Hash_Type;
      with function "=" (X, Y : T) return Boolean;
      with function Image (X : T) return String;
   package States is
      type State is tagged private;

      procedure Gen  (S : in out State; X : T);
      procedure Kill (S : in out State; X : T);

      function Union (X, Y : State) return State;
      function Intersection (X, Y : State) return State;
      function Less_Than (X, Y : State) return Boolean;

      function Image (X : State) return String;

   private

      package Sets is new Ada.Containers.Hashed_Sets (T, Hash, "=", "=");

      type State is tagged record
         Elems : Sets.Set;
      end record;
   end States;

   generic
      with package State_Description is new States (<>);

      with procedure Visit_Assign
        (State : in out State_Description.State;
         Dest  : LAL.Name;
         Val   : LAL.Expr) is null;

      with procedure Visit_Ignore
        (State : in out State_Description.State;
         Expr  : LAL.Expr) is null;

      with procedure Visit_Return
        (State : in out State_Description.State;
         Expr  : LAL.Expr) is null;
   package Analysis is
      type Result is tagged private;

      function Run (Subp : LAL.Base_Subp_Body) return Result;

      function Query
        (R : Result; Node : LAL.Ada_Node) return State_Description.State;

   private
      package State_Maps is new Ada.Containers.Hashed_Maps
        (LAL.Ada_Node,
         State_Description.State,
         LAL.Hash,
         LAL."=",
         State_Description."=");

      type Result is tagged record
         States : State_Maps.Map;
      end record;
   end Analysis;
end Analysis.Dataflow;
