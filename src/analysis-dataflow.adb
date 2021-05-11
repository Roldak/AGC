with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Common;
with Libadalang.Iterators;

with Dot_Printer;

package body Analysis.Dataflow is
   package LALCO renames Libadalang.Common;

   type Node_Handler_Type is access procedure (X : LAL.Ada_Node'Class);

   function Enclosed_Parent (X : LAL.Ada_Node) return LAL.Ada_Node is
      R : LAL.Ada_Node := X.Parent;
   begin
      if R.Kind in LALCO.Ada_Base_Subp_Body then
         return LAL.No_Ada_Node;
      end if;
      return R;
   end Enclosed_Parent;

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
               if PC.As_Ada_List.Children_Count = 0 then
                  return PC;
               else
                  PC := PC.As_Ada_List.Child
                    (PC.As_Ada_List.Last_Child_Index);
               end if;

            when LALCO.Ada_Decl_Block =>
               PC := PC.As_Decl_Block.F_Stmts.As_Ada_Node;

            when LALCO.Ada_Begin_Block =>
               PC := PC.As_Begin_Block.F_Stmts.As_Ada_Node;

            when LALCO.Ada_Return_Stmt
                   | LALCO.Ada_Extended_Return_Stmt
                   | LALCO.Ada_Raise_Stmt
                   | LALCO.Ada_Goto_Stmt
                   | LALCO.Ada_Loop_Stmt =>
               return LAL.No_Ada_Node;

            when LALCO.Ada_If_Stmt =>
               for Alt of PC.As_If_Stmt.F_Alternatives loop
                  Include
                    (Right_Most_Element (Alt, Include));
               end loop;

               Include
                 (Right_Most_Element (PC.As_If_Stmt.F_Else_Stmts, Include));

               PC := PC.As_If_Stmt.F_Then_Stmts.As_Ada_Node;

            when LALCO.Ada_Elsif_Stmt_Part =>
               PC := PC.As_Elsif_Stmt_Part.F_Stmts.As_Ada_Node;

            when LALCO.Ada_For_Loop_Stmt | LALCO.Ada_While_Loop_Stmt =>
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
            for I in 2 .. PC.As_Case_Stmt.F_Alternatives.Children_Count loop
               Include (PC.As_Case_Stmt.F_Alternatives.Child (I));
            end loop;
            PC := PC.As_Case_Stmt.F_Alternatives.Child (1);
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
      elsif PC.Kind in LALCO.Ada_Extended_Return_Stmt then
         PC := PC.As_Extended_Return_Stmt.F_Decl.As_Ada_Node;
      end if;

      while PC.Is_Null loop
         Orig := Enclosed_Parent (Orig);

         if Orig.Is_Null then
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
         when LALCO.Ada_Extended_Return_Stmt =>
            PC := Right_Most_Element
              (PC.As_Extended_Return_Stmt.F_Stmts, Include);
            return;

         when LALCO.Ada_Declarative_Part =>
            PC := Enclosed_Parent (PC);
            return;
         when LALCO.Ada_Ada_List =>
            PC := Enclosed_Parent (PC);
            return;

         when LALCO.Ada_Elsif_Stmt_Part =>
            PC := PC.Parent.Parent;
            return;

         when LALCO.Ada_Case_Stmt_Alternative =>
            PC := PC.Parent.Parent;
            return;

         when LALCO.Ada_Base_Loop_Stmt =>
            Include
              (Right_Most_Element (PC.As_Base_Loop_Stmt.F_Stmts, Include));
            PC := PC.Previous_Sibling;

         when others =>
            PC := PC.Previous_Sibling;
      end case;

      if PC.Is_Null then
         if Orig.Kind in LALCO.Ada_Stmt then
            PC := Orig.Parent;
            return;
         elsif Orig.Kind in LALCO.Ada_Extended_Return_Stmt_Object_Decl then
            PC := Orig.Parent.Previous_Sibling;
            if PC.Is_Null then
               PC := Orig.Parent.Parent;
            end if;
            return;
         elsif Orig.Parent.Parent.Kind in LALCO.Ada_Declarative_Part then
            PC := Orig.Parent;
            return;
         end if;
      end if;

      while PC.Is_Null loop
         Orig := Enclosed_Parent (Orig);

         if Orig.Is_Null then
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
        (Ctx       : LAL.Base_Subp_Body;
         PC        : LAL.Ada_Node;
         Old_State : States.T)
         return States.T
      is
         use all type LAL.Ada_Node;

         New_State : States.T := Old_State;
      begin
         case PC.Kind is
            when LALCO.Ada_Base_Subp_Spec =>
               if PC /= Ctx.F_Subp_Spec then
                  raise Program_Error with "Should not happen";
               end if;

               for Param of PC.As_Base_Subp_Spec.P_Params loop
                  Visit_Parameter (New_State, Ctx, Param);
               end loop;

            when LALCO.Ada_Expr_Function =>
               if PC /= Ctx then
                  return New_State;
               end if;

               Visit_Return
                 (State => New_State,
                  Ctx   => Ctx,
                  Expr  => PC.As_Expr_Function.F_Expr);

            when LALCO.Ada_Object_Decl_Range =>
               if PC.As_Object_Decl.F_Default_Expr.Is_Null then
                  return New_State;
               end if;

               for D of PC.As_Object_Decl.F_Ids loop
                  Visit_Assign
                    (State => New_State,
                     Ctx   => Ctx,
                     Dest  => D.As_Name,
                     Val   => PC.As_Object_Decl.F_Default_Expr);
               end loop;

            when LALCO.Ada_Assign_Stmt =>
               Visit_Assign
                 (State => New_State,
                  Ctx   => Ctx,
                  Dest  => PC.As_Assign_Stmt.F_Dest,
                  Val   => PC.As_Assign_Stmt.F_Expr);

            when LALCO.Ada_Call_Stmt =>
               Visit_Ignore
                 (State => New_State,
                  Ctx   => Ctx,
                  Expr  => PC.As_Call_Stmt.F_Call.As_Expr);

            when LALCO.Ada_Return_Stmt =>
               Visit_Return
                 (State => New_State,
                  Ctx   => Ctx,
                  Expr  => PC.As_Return_Stmt.F_Return_Expr);

            when LALCO.Ada_Extended_Return_Stmt =>
               Visit_Return
                 (State => New_State,
                  Ctx   => Ctx,
                  Expr  => PC.As_Extended_Return_Stmt
                           .F_Decl.P_Defining_Name.As_Expr);

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
         use all type LAL.Ada_Node;

         Iter : Traverse_Iterator'Class := Find
           (Subp,
            Kind_Is (LALCO.Ada_Return_Stmt) or
            Kind_Is (LALCO.Ada_Extended_Return_Stmt));
         Node : LAL.Ada_Node;
      begin
         while Iter.Next (Node) loop
            if Utils.Enclosing_Subp_Body (Node) = Subp then
               Handle (Node);
            end if;
         end loop;
      end Foreach_Return_Stmt;

      function Meet (X, Y : States.T) return States.T is
         use States;
      begin
         return (if Confluence in May then X or Y else X and Y);
      end Meet;

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

      function Empty (Subp : LAL.Base_Subp_Body) return Solution is
        ((Ctx => Subp.As_Body_Node, others => <>));

      function Fixpoint (Subp : LAL.Base_Subp_Body) return Solution is
         R : Solution := (Ctx => Subp.As_Body_Node, others => <>);

         function With_Params (S : States.T) return States.T is
           (Transfer (Subp, Subp.As_Base_Subp_Body.F_Subp_Spec.As_Ada_Node, S));

         Actual_Entry_State : States.T :=
           (if Flow in Forward
            then With_Params (Entry_State)
            else Entry_State);

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

         procedure Add_Entry_Point (X : LAL.Ada_Node'Class) is
         begin
            W.Include (X.As_Ada_Node);
            R.States.Insert (X.As_Ada_Node, Actual_Entry_State);
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
                    (Subp.As_Subp_Body.F_End_Name);
               end if;
            when LALCO.Ada_Expr_Function =>
               R.States.Insert
                 (Subp.As_Ada_Node,
                  Transfer (Subp, Subp.As_Ada_Node, Actual_Entry_State));
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
                     Transfer (Subp, PC, State_Maps.Element (PC_State));

                  function Update (X : LAL.Ada_Node) return Boolean is
                     use type States.T;

                     Inserted : Boolean;
                     X_State  : State_Maps.Cursor := Node_State (X, Inserted);

                     Meet_State : States.T :=
                       (if Inserted
                        then New_State
                        else Meet (State_Maps.Element (X_State), New_State));
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
                     if not X.Is_Null then
                        if Update (X.As_Ada_Node) then
                           W.Include (X.As_Ada_Node);
                        end if;
                     end if;
                  end Include;
               begin
                  R.States.Replace_Element (PC_State, New_State);
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

      procedure Query_After
        (S       : Solution;
         Node    : LAL.Ada_Node'Class;
         Process : access procedure (N : LAL.Ada_Node; V : States.T))
      is
         use State_Maps;

         procedure Callback (X : LAL.Ada_Node'Class) is
         begin
            if not X.Is_Null then
               Process (X.As_Ada_Node, S.States.Element (X.As_Ada_Node));
            end if;
         end Callback;

         PC : LAL.Ada_Node := Node.As_Ada_Node;
      begin
         Next (PC, Callback'Unrestricted_Access);
         if not PC.Is_Null then
            Callback (PC);
         end if;
      end Query_After;

      procedure Query_Before
        (S       : Solution;
         Node    : LAL.Ada_Node'Class;
         Process : access procedure (N : LAL.Ada_Node; V : States.T))
      is
         use State_Maps;

         procedure Callback (X : LAL.Ada_Node'Class) is
         begin
            if not X.Is_Null then
               Process (X.As_Ada_Node, S.States.Element (X.As_Ada_Node));
            end if;
         end Callback;

         PC : LAL.Ada_Node := Node.As_Ada_Node;
      begin
         Prev (PC, Callback'Unrestricted_Access);
         if not PC.Is_Null then
            Callback (PC);
         end if;
      end Query_Before;

      procedure Query_End_States
        (S       : Solution;
         Process : access procedure (N : LAL.Ada_Node; V : States.T))
      is
         procedure Process_Node (N : LAL.Ada_Node'Class) is
            use type State_Maps.Cursor;

            Cursor : constant State_Maps.Cursor :=
               S.States.Find (N.As_Ada_Node);
         begin
            if Cursor /= State_Maps.No_Element then
               Process (N.As_Ada_Node, State_Maps.Element (Cursor));
            end if;
         end Process_Node;
      begin
         case S.Ctx.Kind is
            when LALCO.Ada_Subp_Body =>
               if Flow in Forward then
                  Foreach_Return_Stmt
                    (S.Ctx.As_Base_Subp_Body, Process_Node'Unrestricted_Access);
                  Process_Node
                    (S.Ctx.As_Subp_Body.F_End_Name);
               else
                  Process_Node (S.Ctx.As_Subp_Body.F_Decls);
               end if;
            when LALCO.Ada_Expr_Function =>
               Process_Node (S.Ctx);
            when LALCO.Ada_Null_Subp_Decl =>
               return;
            when others =>
               raise Program_Error with "Unexpected subprogram kind.";
         end case;
      end Query_End_States;

      procedure Iterate
        (S : Solution;
         F : access procedure (N : LAL.Ada_Node; V : States.T))
      is
         procedure F_Adapter (C : State_Maps.Cursor) is
         begin
            F (State_Maps.Key (C), State_Maps.Element (C));
         end F_Adapter;
      begin
         S.States.Iterate (F_Adapter'Access);
      end Iterate;

      procedure Dump (S : Solution) is
         use State_Maps;
         use Langkit_Support.Text;

         procedure Dump_One (C : Cursor) is
         begin
            Put_Line ("State of " & LAL.Image (State_Maps.Key (C)) & " : ");
            Put_Line ("   " & States.Image (Element (C)));
         end Dump_One;
      begin
         S.States.Iterate (Dump_One'Access);
      end Dump;

      procedure Output_Graph (S : Solution; Path : String) is
         use Langkit_Support.Text;

         Printer : Dot_Printer.Printer;

         function Hash
           (X : LAL.Ada_Node'Class) return Ada.Containers.Hash_Type
         is
            use type Ada.Containers.Hash_Type;
         begin
            if X.Is_Null then
               return 0;
            else
               return LAL.Hash (X) + Ada.Strings.Hash (LAL.Image (X));
            end if;
         end Hash;

         function Name
           (X : LAL.Ada_Node'Class;
            S : States.T) return Unbounded_Text_Type
         is
            State_Img : Text_Type := To_Text (States.Image (S));
         begin
            if X.Kind in LALCO.Ada_Simple_Stmt | LALCO.Ada_Object_Decl then
               return To_Unbounded_Text (LAL.Text (X) & "\n\n" & State_Img);
            else
               return To_Unbounded_Text (State_Img);
            end if;
         end Name;

         procedure Write_Node (C : State_Maps.Cursor) is
            Orig  : constant LAL.Ada_Node := State_Maps.Key (C);
            State : constant States.T := State_Maps.Element (C);

            procedure Add_Edge (N : LAL.Ada_Node'Class) is
            begin
               if not N.Is_Null then
                  Printer.Add_Edge  (Hash (Orig), Hash (N));
               end if;
            end Add_Edge;

            Node : LAL.Ada_Node := Orig;
         begin
            Printer.Add_Node
              (Hash (Orig),
               Name (Orig, State),
               Dot_Printer.Empty_Path);

            Flow_Next (Node, Add_Edge'Unrestricted_Access);
            Add_Edge (Node);
         end Write_Node;
      begin
         S.States.Iterate (Write_Node'Access);
         Printer.Save (Path);
      end Output_Graph;
   end Problem;
end Analysis.Dataflow;
