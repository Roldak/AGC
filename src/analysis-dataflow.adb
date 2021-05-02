with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Common;
with Libadalang.Iterators;

package body Analysis.Dataflow is
   package LALCO renames Libadalang.Common;

   type Node_Handler_Type is access procedure (X : LAL.Ada_Node'Class);

   function Right_Most_Element
     (Orig    : LAL.Ada_Node'Class;
      Include : Node_Handler_Type)
      return LAL.Ada_Node
   is
      PC : LAL.Ada_Node := Orig.As_Ada_Node;
   begin
      loop
         case PC.Kind is
            when LALCO.Ada_Declarative_Part =>
               PC := PC.As_Declarative_Part.F_Decls.As_Ada_Node;
            when LALCO.Ada_Handled_Stmts =>
               PC := PC.As_Handled_Stmts.F_Stmts.As_Ada_Node;
            when LALCO.Ada_Ada_List =>
               PC := PC.As_Ada_List.Child
                 (PC.As_Ada_List.Last_Child_Index);

            when LALCO.Ada_Return_Stmt
                   | LALCO.Ada_Raise_Stmt
                   | LALCO.Ada_Goto_Stmt =>
               return LAL.No_Ada_Node;

            when LALCO.Ada_If_Stmt =>
               for Alt of PC.As_If_Stmt.F_Alternatives loop
                  Include
                    (Right_Most_Element (Alt, Include));
               end loop;
               Include
                 (Right_Most_Element (PC.As_If_Stmt.F_Else_Stmts, Include));
               PC := PC.As_If_Stmt.F_Then_Stmts.As_Ada_Node;

            when LALCO.Ada_Base_Loop_Stmt =>
               if PC.Kind in LALCO.Ada_Loop_Stmt then
                  return LAL.No_Ada_Node;
               end if;
               return PC;

            when others =>
               return PC;
         end case;
      end loop;
   end Right_Most_Element;

   procedure Next
     (PC      : in out LAL.Ada_Node;
      Include : Node_Handler_Type)
   is
      Orig : LAL.Ada_Node := PC;
   begin
      case PC.Kind is
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
            for Alt of PC.As_If_Stmt.F_Alternatives loop
               Include (Alt);
            end loop;
            Include (PC.As_If_Stmt.F_Else_Stmts);
            PC := PC.As_If_Stmt.F_Then_Stmts.As_Ada_Node;
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
               PC := Orig.Parent.Parent;
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

   procedure Prev
     (PC      : in out LAL.Ada_Node;
      Include : Node_Handler_Type)
   is
      Orig : LAL.Ada_Node := PC;
   begin
      case PC.Kind is
         when LALCO.Ada_Declarative_Part =>
            PC := PC.Parent;
            return;
         when LALCO.Ada_Ada_List =>
            PC := PC.Parent;
            return;

         when LALCO.Ada_Elsif_Stmt_Part =>
            PC := PC.Parent;
            return;

         when LALCO.Ada_Base_Loop_Stmt =>
            Include (Right_Most_Element (PC, Include));
            PC := PC.Previous_Sibling;

         when others =>
            PC := PC.Previous_Sibling;
      end case;

      if PC.Is_Null then
         if Orig.Kind in LALCO.Ada_Stmt then
            PC := Orig.Parent;
            return;
         end if;
      end if;

      while PC.Is_Null loop
         Orig := Orig.Parent;

         if Orig.Kind in LALCO.Ada_Base_Subp_Body then
            return;
         end if;

         PC := Orig.Previous_Sibling;
      end loop;

      PC := Right_Most_Element (PC, Include);
   end Prev;

   package body Problem is
      package Node_Sets is new Ada.Containers.Hashed_Sets
        (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

      function Transfer
        (PC        : LAL.Ada_Node;
         Old_State : States.T)
         return States.T
      is
         New_State : States.T := Old_State;
      begin
         case PC.Kind is
            when LALCO.Ada_Object_Decl =>
               for D of PC.As_Object_Decl.F_Ids loop
                  Visit_Assign
                    (State => New_State,
                     Dest  => D.As_Name,
                     Val   => PC.As_Object_Decl.F_Default_Expr);
               end loop;
            when LALCO.Ada_Assign_Stmt =>
               Visit_Assign
                 (State => New_State,
                  Dest  => PC.As_Assign_Stmt.F_Dest,
                  Val   => PC.As_Assign_Stmt.F_Expr);
            when LALCO.Ada_Call_Stmt =>
               Visit_Ignore
                 (State => New_State,
                  Expr  => PC.As_Call_Stmt.F_Call.As_Expr);
            when LALCO.Ada_Return_Stmt =>
               Visit_Return
                 (State => New_State,
                  Expr  => PC.As_Return_Stmt.F_Return_Expr);
            when LALCO.Ada_Expr_Function =>
               Visit_Return
                 (State => New_State,
                  Expr  => PC.As_Expr_Function.F_Expr);
            when others =>
               null;
         end case;
         return New_State;
      end Transfer;

      procedure Foreach_Return_Stmt
        (Subp : LAL.Base_Subp_Body;
         Handle : Node_Handler_Type)
      is
         use Libadalang.Iterators;

         Iter : Traverse_Iterator'Class :=
            Find (Subp, Kind_Is (LALCO.Ada_Return_Stmt));
         Node : LAL.Ada_Node;
      begin
         while Iter.Next (Node) loop
            Handle (Node);
         end loop;
      end Foreach_Return_Stmt;

      function Fixpoint (Subp : LAL.Base_Subp_Body) return Solution is
         R  : Solution;

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

         function Op (X, Y : States.T) return States.T is
            use States;
         begin
            return (if Confluence in May then X or Y else X and Y);
         end Op;

         procedure Flow_Next
           (PC      : in out LAL.Ada_Node;
            Include : Node_Handler_Type)
         is
         begin
            if Flow in Forward then
               Next (PC, Include);
            else
               Prev (PC, Include);
            end if;
         end Flow_Next;

         W  : Node_Sets.Set;

         procedure Add_Entry_Point (X : LAL.Ada_Node'Class) is
         begin
            W.Include (X.As_Ada_Node);
            R.States.Insert (X.As_Ada_Node, Entry_State);
         end Add_Entry_Point;

         PC : LAL.Ada_Node;
      begin
         case Subp.Kind is
            when LALCO.Ada_Subp_Body =>
               if Flow in Forward then
                  Add_Entry_Point (Subp.As_Subp_Body.F_Decls);
               else
                  Foreach_Return_Stmt
                    (Subp, Add_Entry_Point'Unrestricted_Access);
                  Add_Entry_Point
                    (Right_Most_Element
                       (Subp.As_Subp_Body.F_Stmts,
                        Add_Entry_Point'Unrestricted_Access));
               end if;
            when LALCO.Ada_Expr_Function =>
               R.States.Insert
                 (Subp.As_Ada_Node,
                  Transfer (Subp.As_Ada_Node, Entry_State));
               return R;
            when LALCO.Ada_Null_Subp_Decl =>
               return R;
            when others =>
               raise Program_Error with "Unexpected subprogram kind.";
         end case;

         while not W.Is_Empty loop
            PC := Node_Sets.Element (W.First);
            loop
               W.Exclude (PC);
               declare
                  Dummy     : Boolean;
                  PC_State  : State_Maps.Cursor := Node_State (PC, Dummy);
                  New_State : States.T :=
                     Transfer (PC, State_Maps.Element (PC_State));

                  function Update (X : LAL.Ada_Node) return Boolean is
                     use type States.T;

                     Inserted : Boolean;
                     X_State  : State_Maps.Cursor := Node_State (X, Inserted);

                     Meet_State : States.T :=
                       (if Inserted
                        then New_State
                        else Op (State_Maps.Element (X_State), New_State));
                  begin
                     if Inserted or else
                        Meet_State /= State_Maps.Element (X_State)
                     then
                        R.States.Replace_Element (X_State, Meet_State);
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
                  Flow_Next (PC, Include'Unrestricted_Access);
                  exit when PC.Is_Null or else not Update (PC);
               end;
            end loop;
         end loop;
         return R;
      end Fixpoint;

      function Query_At
        (S : Solution; Node : LAL.Ada_Node) return States.T
      is
      begin
         return S.States.Element (Node);
      end Query_At;

      function Query_After
        (S : Solution; Node : LAL.Ada_Node) return States.T
      is
         use State_Maps;

         procedure Do_Nothing (X : LAL.Ada_Node'Class) is null;

         PC : LAL.Ada_Node := Node;
      begin
         Next (PC, Do_Nothing'Unrestricted_Access);
         return S.States.Element (PC);
      end Query_After;
   end Problem;
end Analysis.Dataflow;
