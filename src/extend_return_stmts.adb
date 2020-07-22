with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Hashed_Maps;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

with Node_Counters;
with Utils;

procedure Extend_Return_Stmts
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Handle_Return_Stmt (Stmt : LAL.Return_Stmt'Class) is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      Ret_Expr : LAL.Expr := Stmt.F_Return_Expr;

      Subp     : LAL.Base_Subp_Body := Utils.Enclosing_Subp_Body (Stmt);
      Ret_Type : LAL.Type_Expr := Subp.F_Subp_Spec.F_Subp_Returns;
   begin
      LALRW.Replace
        (LALRW.Handle (Stmt),
         LALRW.Create_From_Template
           (RH,
            "return AGC_Ret : {} := {} do null; end return;",
            (1 => LALRW.Clone (LALRW.Handle (Ret_Type)),
             2 => LALRW.Clone (LALRW.Handle (Ret_Expr))),
            LALCO.Ext_Return_Stmt_Rule));
   end Handle_Return_Stmt;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Return_Stmt =>
            Handle_Return_Stmt (Node.As_Return_Stmt);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
   if not LALRW.Apply (RH).Success then
      raise Program_Error with "extend_return_stmts: could not apply rewritings";
   end if;
end Extend_Return_Stmts;
