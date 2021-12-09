with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
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

   function "<" (A, B : LAL.Ada_Node) return Boolean is
     (Start_Sloc (A.Sloc_Range) < Start_Sloc (B.Sloc_Range));

   package Sorted_Node_Sets is new Ada.Containers.Ordered_Sets
     (LAL.Ada_Node, "<", LAL."=");

   function To_Sorted_Set (X : Node_Sets.Set) return Sorted_Node_Sets.Set is
      R : Sorted_Node_Sets.Set;
   begin
      for E of X loop
         R.Insert (E);
      end loop;
      return R;
   end To_Sorted_Set;

   package Node_Handle_Maps is new Ada.Containers.Hashed_Maps
     (LAL.Ada_Node, LALRW.Node_Rewriting_Handle, LAL.Hash, LAL."=", LALRW."=");

   Return_Stmt_Map : Node_Handle_Maps.Map;

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

      function Get_Extended_Return_Stmt
        (Original : LAL.Return_Stmt) return LALRW.Node_Rewriting_Handle
      is
         use type Node_Handle_Maps.Cursor;

         Cursor : Node_Handle_Maps.Cursor :=
            Return_Stmt_Map.Find (Original.As_Ada_Node);
      begin
         if Cursor = Node_Handle_Maps.No_Element then
            declare
               Subp : constant LAL.Base_Subp_Body :=
                  Utils.Enclosing_Subp_Body (Original);

               Ret_Type : constant LAL.Type_Expr :=
                  Subp.F_Subp_Spec.F_Subp_Returns;

               Ret_Expr : constant LAL.Expr :=
                  Original.F_Return_Expr;

               ERH : LALRW.Node_Rewriting_Handle :=
                  LALRW.Create_From_Template
                    (RH,
                     "return AGC_Ret : {} := {} do {} end return;",
                     (1 => LALRW.Handle (Ret_Type),
                      2 => LALRW.Handle (Ret_Expr),
                      3 => LALRW.Create_Node (RH, LALCO.Ada_Stmt_List)),
                     LALCO.Ext_Return_Stmt_Rule);

               Dummy_Inserted : Boolean;
            begin
               LALRW.Replace (LALRW.Handle (Original), ERH);
               Return_Stmt_Map.Insert
                 (Original.As_Ada_Node, ERH, Cursor, Dummy_Inserted);
            end;
         end if;
         return Node_Handle_Maps.Element (Cursor);
      end Get_Extended_Return_Stmt;

      procedure Insert_After_Return
        (Original : LAL.Return_Stmt)
      is
         ERH : constant LALRW.Node_Rewriting_Handle :=
            Get_Extended_Return_Stmt (Original);

         Stmt_List : constant LALRW.Node_Rewriting_Handle :=
            LALRW.Child (LALRW.Child (ERH, 2), 1);
      begin
         Insert_At (Stmt_List, 1);
      end Insert_After_Return;

      procedure Unexpected is
      begin
         raise Program_Error with "Could not insert free statement";
      end Unexpected;
   begin
      case From.Kind is
         when LALCO.Ada_Simple_Stmt
               | LALCO.Ada_Object_Decl
               | LALCO.Ada_Case_Stmt
               | LALCO.Ada_While_Loop_Stmt =>
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
         when LALCO.Ada_Extended_Return_Stmt_Object_Decl =>
            Insert_At (LALRW.Handle (To.As_Handled_Stmts.F_Stmts), 1);
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
   begin
      if From.Parent.Parent.Kind in LALCO.Ada_Declarative_Part then
         Insert_Node_Between
           (From, To,
            LALRW.Create_From_Template
              (RH,
               "AGC_Free_" & Var.Text
               & " : AGC.Empty_Type := AGC.Free (" & Var.Text & ");",
               (1 .. 0 => <>), LALCO.Object_Decl_Rule));
      else
         Insert_Node_Between
           (From, To,
            LALRW.Create_From_Template
              (RH, "AGC.Free (" & Var.Text & ");",
               (1 .. 0 => <>), LALCO.Stmt_Rule));
      end if;
   end Generate_Free_Between;

   function Is_Parameter (Var : LAL.Defining_Name) return Boolean is
     (Var.P_Basic_Decl.Kind in LALCO.Ada_Param_Spec);

   function Without_Returned_Value
     (Stmt : LAL.Return_Stmt; State : Node_Sets.Set) return Node_Sets.Set
   is
      Returned_Expr : constant LAL.Expr := Stmt.F_Return_Expr;
      Refd_Name     : constant LAL.Defining_Name :=
        (if Returned_Expr.Kind in LALCO.Ada_Identifier
         then Returned_Expr.As_Identifier.P_Referenced_Defining_Name
         else LAL.No_Defining_Name);

      New_State : Node_Sets.Set := State;
   begin
      if not Refd_Name.Is_Null then
         New_State.Exclude (Refd_Name.P_Canonical_Part.As_Ada_Node);
      end if;
      return New_State;
   end Without_Returned_Value;

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
            for Var of To_Sorted_Set (R.Difference (S)) loop
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
              (N, Without_Returned_Value (N.As_Return_Stmt, S),
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
