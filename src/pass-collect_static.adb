with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Iterators;
with Libadalang.Rewriting;

with Analysis;
with Analysis.Lattices.Finite_Node_Sets;
with Analysis.Dataflow;
with Analysis.Ownership;
with Session;
with Utils;

procedure Pass.Collect_Static
  (Job_Ctx  : Libadalang.Helpers.App_Job_Context;
   Unit     : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALI    renames Libadalang.Iterators;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   package Node_Sets renames Analysis.Lattices.Finite_Node_Sets.Node_Sets;

   use all type LAL.Ada_Node;

   --  Rewriting handle is created lazily in this phase because many units
   --  won't need to be rewritten.
   RH : LALRW.Rewriting_Handle := LALRW.No_Rewriting_Handle;

   procedure Start_Rewriting is
      use type LALRW.Rewriting_Handle;
   begin
      if RH = LALRW.No_Rewriting_Handle then
         RH := LALRW.Start_Rewriting (Unit.Context);
      end if;
   end Start_Rewriting;

   function Apply_Rewritings_If_Relevant return Boolean is
      use type LALRW.Rewriting_Handle;
   begin
      if RH /= LALRW.No_Rewriting_Handle then
         return LALRW.Apply (RH).Success;
      end if;
      return True;
   end Apply_Rewritings_If_Relevant;

   procedure Add_All_References
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Expr  : LAL.Expr)
   is
      function Add_Ref (X : LAL.Ada_Node'Class) return LALCO.Visit_Status is
         Ref : LAL.Defining_Name;
      begin
         case X.Kind is
            when LALCO.Ada_Identifier =>
               Ref := X.As_Name.P_Referenced_Defining_Name;
               if not Ref.Is_Null then
                  if Utils.Enclosing_Subp_Body (Ref) = Ctx then
                     State.Include (Ref.As_Ada_Node);
                  end if;
               end if;
            when others =>
               null;
         end case;
         return LALCO.Into;
      exception
         when LALCO.Property_Error =>
            return LALCO.Over;
      end Add_Ref;
   begin
      Expr.Traverse (Add_Ref'Access);
   end Add_All_References;

   procedure Handle_Assignment
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Dest  : LAL.Name;
      Val   : LAL.Expr)
   is
      D : LAL.Defining_Name :=
        (if Dest.P_Is_Defining
         then Dest.P_Enclosing_Defining_Name
         else Dest.P_Referenced_Defining_Name);
   begin
      Add_All_References (State, Ctx, Val);

      if Utils.Enclosing_Subp_Body (D) = Ctx then
         State.Exclude (D.As_Ada_Node);
      end if;
   end Handle_Assignment;

   package Liveness_Problem is new Analysis.Dataflow.Problem
     (States       => Analysis.Lattices.Finite_Node_Sets.Lattice,
      Confluence   => Analysis.Dataflow.May,
      Flow         => Analysis.Dataflow.Backwards,
      Visit_Assign => Handle_Assignment,
      Visit_Ignore => Add_All_References,
      Entry_State  => Node_Sets.Empty_Set);

   procedure Insert_Node_Between
     (From : LAL.Ada_Node;
      To   : LAL.Ada_Node;
      Stmt : LALRW.Node_Rewriting_Handle)
   is
      use type LAL.Ada_Node;

      procedure Insert_At
        (List  : LALRW.Node_Rewriting_Handle;
         Index : Positive)
      is
      begin
         LALRW.Insert_Child (List, Index, Stmt);
      end Insert_At;

      procedure Unexpected is
      begin
         raise Program_Error with "Could not insert free statement";
      end Unexpected;
   begin
      case From.Kind is
         when LALCO.Ada_Simple_Stmt =>
            Insert_At
              (LALRW.Handle (From.Parent),
               Utils.Child_Index (LALRW.Handle (From)) + 1);
         when LALCO.Ada_If_Stmt =>
            if From = To.Parent then
               Insert_At (LALRW.Handle (To), 1);
            elsif From.As_If_Stmt.F_Else_Stmts.Children_Count = 0 then
               Insert_At (LALRW.Handle (From.As_If_Stmt.F_Else_Stmts), 1);
            else
               Unexpected;
            end if;
         when others =>
            Unexpected;
      end case;
   end Insert_Node_Between;

   procedure Generate_Free_Between
     (From : LAL.Ada_Node;
      To   : LAL.Ada_Node;
      Var  : LAL.Defining_Name)
   is
      Stmt : LALRW.Node_Rewriting_Handle;
   begin
      Start_Rewriting;
      Stmt := LALRW.Create_From_Template
        (RH, "AGC.Free (" & Var.Text & ");",
         (1 .. 0 => <>), LALCO.Stmt_Rule);
      Insert_Node_Between (From, To, Stmt);
   end Generate_Free_Between;

   procedure Handle_Subp_Body (Subp : LAL.Base_Subp_Body) is
      Liveness_Result  : constant Liveness_Problem.Solution :=
         Liveness_Problem.Fixpoint (Subp);

      Ownership_Result : constant Analysis.Ownership.Problem.Solution :=
         Analysis.Ownership.Share.Get_Context_Solution (Subp.As_Body_Node);

      procedure Detect_Death
        (N : LAL.Ada_Node; S : Node_Sets.Set)
      is
         use Analysis.Lattices;

         Total : Node_Sets.Set;

         procedure Compare_To (M : LAL.Ada_Node; R : Node_Sets.Set) is
            use Finite_Node_Sets.Lattice;

            Owners : constant Node_Sets.Set := Ownership_Result.Query_At (M);
         begin
            if not Leq (R, S) then
               for Var of R.Difference (S) loop
                  if Owners.Contains (Var) then
                     Generate_Free_Between (M, N, Var.As_Defining_Name);
                  end if;
               end loop;
            end if;
         end Compare_To;
      begin
         Liveness_Result.Query_Before (N, Compare_To'Access);
      end Detect_Death;
   begin
      Liveness_Result.Iterate (Detect_Death'Access);
   end Handle_Subp_Body;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Base_Subp_Body =>
            Handle_Subp_Body (Node.As_Base_Subp_Body);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   if Session.Get_Optimization_Level not in Session.Full then
      return;
   end if;

   Unit.Root.Traverse (Process_Node'Access);
   if not Apply_Rewritings_If_Relevant then
      raise Program_Error with "collect_static: could not apply rewritings";
   end if;
end Pass.Collect_Static;
