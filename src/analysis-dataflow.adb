with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with Libadalang.Common;

package body Analysis.Dataflow is
   package LALCO renames Libadalang.Common;

   package body States is
      procedure Gen  (S : in out State; X : T) is
      begin
         S.Elems.Include (X);
      end Gen;

      procedure Kill (S : in out State; X : T) is
      begin
         S.Elems.Exclude (X);
      end Kill;

      function Union (X, Y : State) return State is
      begin
         return (Elems => X.Elems.Union (Y.Elems));
      end Union;

      function Intersection (X, Y : State) return State is
      begin
         return (Elems => X.Elems.Intersection (Y.Elems));
      end Intersection;

      function Less_Than (X, Y : State) return Boolean is
         use type Ada.Containers.Count_Type;
      begin
         return
            X.Elems.Is_Subset (Y.Elems) and then
            X.Elems.Length < Y.Elems.Length;
      end Less_Than;

      function Image (X : State) return String is
         use Ada.Strings.Unbounded;

         R : Unbounded_String;
      begin
         Append (R, "{");
         for E of X.Elems loop
            Append (R, Image (E));
         end loop;
         Append (R, "}");
         return To_String (R);
      end Image;
   end States;

   package body Analysis is
      package Node_Sets is new Ada.Containers.Hashed_Sets
        (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

      type Node_Handler_Type is access procedure (X : LAL.Ada_Node'Class);

      procedure Next
        (PC      : in out LAL.Ada_Node;
         Include : Node_Handler_Type)
      is
         Orig : LAL.Ada_Node := PC;
      begin
         case PC.Kind is
            when LALCO.Ada_Subp_Body =>
               PC := PC.As_Subp_Body.F_Decls.As_Ada_Node;

            when LALCO.Ada_Declarative_Part =>
               PC := PC.As_Declarative_Part.F_Decls.As_Ada_Node;

            when LALCO.Ada_Handled_Stmts =>
               PC := PC.As_Handled_Stmts.F_Stmts.As_Ada_Node;

            when LALCO.Ada_Ada_List =>
               PC := PC.Child (1);

            when LALCO.Ada_Return_Stmt =>
               PC := LAL.No_Ada_Node;
               return;

            when LALCO.Ada_Raise_Stmt =>
               PC := LAL.No_Ada_Node;
               return;

            when LALCO.Ada_Goto_Stmt =>
               PC :=
                  PC.As_Goto_Stmt.F_Label_Name.P_Referenced_Decl.As_Ada_Node;

            when LALCO.Ada_If_Stmt =>
               PC := PC.As_If_Stmt.F_Then_Stmts.As_Ada_Node;
               for Alt of PC.As_If_Stmt.F_Alternatives loop
                  Include (Alt);
               end loop;
               Include (PC.As_If_Stmt.F_Else_Stmts);
            when LALCO.Ada_Elsif_Stmt_Part =>
               PC := PC.As_Elsif_Stmt_Part.F_Stmts.As_Ada_Node;

            when LALCO.Ada_Case_Stmt =>
               PC := PC.As_Case_Stmt.F_Alternatives.Child (1);
               for I in 2 .. PC.As_Case_Stmt.F_Alternatives.Children_Count loop
                  Include (PC.As_Case_Stmt.F_Alternatives.Child (I));
               end loop;
            when LALCO.Ada_Case_Stmt_Alternative =>
               PC := PC.As_Case_Stmt_Alternative.F_Stmts.As_Ada_Node;

            when LALCO.Ada_Loop_Stmt =>
               PC := PC.As_Loop_Stmt.F_Stmts.As_Ada_Node;
            when LALCO.Ada_For_Loop_Stmt | LALCO.Ada_While_Loop_Stmt =>
               Include (PC.As_Base_Loop_Stmt.F_Stmts);
               PC := PC.Next_Sibling;

            when LALCO.Ada_Named_Stmt =>
               PC := PC.As_Named_Stmt.F_Stmt.As_Ada_Node;

            when LALCO.Ada_Decl_Block =>
               PC := PC.As_Decl_Block.F_Decls.As_Ada_Node;
            when LALCO.Ada_Begin_Block =>
               PC := PC.As_Begin_Block.F_Stmts.As_Ada_Node;

            when others =>
               PC := PC.Next_Sibling;
         end case;

         if PC.Is_Null then
            if Orig.Kind in LALCO.Ada_Stmt then
               if Orig.Parent.Parent.Kind in
                     LALCO.Ada_If_Stmt
                   | LALCO.Ada_Case_Stmt
               then
                  Orig := Orig.Parent;
               elsif Orig.Parent.Parent.Kind in LALCO.Ada_Elsif_Stmt_Part then
                  Orig := Orig.Parent.Parent;
               elsif Orig.Parent.Parent.Kind in LALCO.Ada_Base_Loop_Stmt then
                  Include (Orig.Parent.Parent.As_Base_Loop_Stmt.F_Stmts);
                  Orig := Orig.Parent;
               end if;
            end if;
         end if;

         while PC.Is_Null loop
            Orig := Orig.Parent;

            if Orig.Kind in LALCO.Ada_Base_Subp_Body then
               return;
            end if;

            PC := Orig.Next_Sibling;
         end loop;
      end Next;

      function Transfer
        (PC        : LAL.Ada_Node;
         Old_State : State_Description.State)
         return State_Description.State
      is
         New_State : State_Description.State := Old_State;
      begin
         case PC.Kind is
            when LALCO.Ada_Object_Decl =>
               Visit_Assign
                 (State => New_State,
                  Dest  => PC.As_Object_Decl.P_Defining_Name.As_Name,
                  Val   => PC.As_Object_Decl.F_Default_Expr);
            when LALCO.Ada_Assign_Stmt =>
               Visit_Assign
                 (State => New_State,
                  Dest  => PC.As_Assign_Stmt.F_Dest,
                  Val   => PC.As_Assign_Stmt.F_Expr);
            when LALCO.Ada_Return_Stmt =>
               Visit_Return
                 (State => New_State,
                  Expr  => PC.As_Return_Stmt.F_Return_Expr);
            when others =>
               null;
         end case;
         return New_State;
      end Transfer;

      function Run (Subp : LAL.Base_Subp_Body) return Result is
         subtype State is State_Description.State;

         R  : Result;

         function Node_State
           (Node     : LAL.Ada_Node;
            Inserted : in out Boolean) return State_Maps.Cursor
         is
            use State_Maps;

            C : Cursor := R.States.Find (Node);
         begin
            Inserted := False;
            if C = No_Element then
               R.States.Insert (Node, C, Inserted);
            end if;
            return C;
         end Node_State;

         W  : Node_Sets.Set;
         PC : LAL.Ada_Node;
      begin
         W.Include (Subp.As_Ada_Node);
         while not W.Is_Empty loop
            PC := Node_Sets.Element (W.First);
            loop
               W.Exclude (PC);
               declare
                  Dummy     : Boolean;
                  PC_State  : State_Maps.Cursor := Node_State (PC, Dummy);
                  New_State : State :=
                     Transfer (PC, State_Maps.Element (PC_State));

                  function Update (X : LAL.Ada_Node) return Boolean is
                     Inserted : Boolean;
                     X_State  : State_Maps.Cursor := Node_State (X, Inserted);
                  begin
                     if Inserted or else
                        New_State.Less_Than (State_Maps.Element (X_State))
                     then
                        R.States.Replace_Element (X_State, New_State);
                        return True;
                     end if;
                     return False;
                  end Update;

                  procedure Include (X : LAL.Ada_Node'Class) is
                  begin
                     if Update (X.As_Ada_Node) then
                        W.Include (X.As_Ada_Node);
                     end if;
                  end Include;
               begin
                  R.States.Replace_Element (PC_State, New_State);
                  Next (PC, Include'Unrestricted_Access);
                  exit when PC.Is_Null or else not Update (PC);
               end;
            end loop;
         end loop;
         return R;
      end Run;

      function Query
        (R : Result; Node : LAL.Ada_Node) return State_Description.State
      is
      begin
         return R.States.Element (Node);
      end Query;
   end Analysis;
end Analysis.Dataflow;
