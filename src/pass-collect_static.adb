with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Helpers;
with Libadalang.Iterators;

with Analysis.Dataflow;
with Analysis.Lattices.Finite_Node_Sets;
with Analysis.Ownership;
with Analysis.Liveness;
with Session;
with Utils;

procedure Pass.Collect_Static
  (Job_Ctx  : Libadalang.Helpers.App_Job_Context;
   Unit     : Libadalang.Analysis.Analysis_Unit)
is
   package LALI    renames Libadalang.Iterators;
   package Node_Sets renames Analysis.Lattices.Finite_Node_Sets.Node_Sets;

   procedure Insert_Node_Between
     (From : LAL.Ada_Node;
      To   : LAL.Ada_Node;
      Stmt : LALRW.Node_Rewriting_Handle)
   is
      use type LAL.Ada_Node;

      RH : LALRW.Rewriting_Handle := Rewriting_Handle (Unit);

      procedure Insert_At
        (List  : LALRW.Node_Rewriting_Handle;
         Index : Positive)
      is
      begin
         LALRW.Insert_Child (List, Index, Stmt);
      end Insert_At;

      procedure Insert_After_Return
        (Original : LAL.Return_Stmt)
      is
         Subp : constant LAL.Base_Subp_Body :=
            Utils.Enclosing_Subp_Body (Original);

         Ret_Type : constant LAL.Type_Expr := Subp.F_Subp_Spec.F_Subp_Returns;
         Ret_Expr : constant LAL.Expr := Original.F_Return_Expr;

         Stmt_List : constant LALRW.Node_Rewriting_Handle :=
            LALRW.Create_Node (RH, LALCO.Ada_Stmt_List);
      begin
         Insert_At (Stmt_List, 1);

         LALRW.Replace
           (LALRW.Handle (Original),
            LALRW.Create_From_Template
              (RH,
               "return AGC_Ret : {} := {} do {} end return;",
               (1 => LALRW.Clone (LALRW.Handle (Ret_Type)),
                2 => LALRW.Clone (LALRW.Handle (Ret_Expr)),
                3 => Stmt_List),
               LALCO.Ext_Return_Stmt_Rule));
      end Insert_After_Return;

      procedure Unexpected is
      begin
         raise Program_Error with "Could not insert free statement";
      end Unexpected;
   begin
      case From.Kind is
         when LALCO.Ada_Simple_Stmt | LALCO.Ada_Case_Stmt =>
            if From.Kind in LALCO.Ada_Return_Stmt then
               Insert_After_Return (From.As_Return_Stmt);
            else
               Insert_At
                 (LALRW.Handle (From.Parent),
                  Utils.Child_Index (LALRW.Handle (From)) + 1);
            end if;
         when LALCO.Ada_If_Stmt =>
            if From = To.Parent then
               Insert_At (LALRW.Handle (To), 1);
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
      RH : LALRW.Rewriting_Handle := Rewriting_Handle (Unit);

      Stmt : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
        (RH, "AGC.Free (" & Var.Text & ");",
         (1 .. 0 => <>), LALCO.Stmt_Rule);
   begin
      Insert_Node_Between (From, To, Stmt);
   end Generate_Free_Between;

   function Is_Parameter (Var : LAL.Defining_Name) return Boolean is
     (Var.P_Basic_Decl.Kind in LALCO.Ada_Param_Spec);

   procedure Handle_Subp_Body (Subp : LAL.Base_Subp_Body) is
      Liveness_Result  : constant Analysis.Liveness.Problem.Solution :=
         Analysis.Liveness.Share.Get_Context_Solution (Subp.As_Body_Node);

      Ownership_Result : constant Analysis.Ownership.Problem.Solution :=
         Analysis.Ownership.Share.Get_Context_Solution (Subp.As_Body_Node);

      procedure Compare_Between
        (M : LAL.Ada_Node; R : Node_Sets.Set;
         N : LAL.Ada_Node; S : Node_Sets.Set)
      is
         use Analysis.Lattices;
         use Finite_Node_Sets.Lattice;

         Owners : constant Node_Sets.Set := Ownership_Result.Query_At (M);

         procedure Kill (Var : LAL.Defining_Name) is
         begin
            if not Is_Parameter (Var) then
               if Owners.Contains (Var.As_Ada_Node) then
                  Generate_Free_Between (M, N, Var.As_Defining_Name);
               end if;
            end if;
         end Kill;
      begin
         if not Leq (R, S) then
            for Var of R.Difference (S) loop
               Kill (Var.As_Defining_Name);
            end loop;
         end if;
      end Compare_Between;

      procedure Detect_Death
        (N : LAL.Ada_Node; S : Node_Sets.Set)
      is
         procedure Compare_To (M : LAL.Ada_Node; R : Node_Sets.Set) is
         begin
            Compare_Between (M, R, N, S);
         end Compare_To;
      begin
         if N.Kind in LALCO.Ada_Return_Stmt then
            Compare_Between
              (N, S,
               LAL.No_Ada_Node, Node_Sets.Empty_Set);
         else
            Liveness_Result.Query_Before (N, Compare_To'Access);
         end if;
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
end Pass.Collect_Static;
