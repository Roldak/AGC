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

procedure Unsugar_Expr_Functions
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit    : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Handle_Expr_Function (Fun : LAL.Expr_Function'Class) is
   begin
      LALRW.Replace
        (LALRW.Handle (Fun),
         LALRW.Create_From_Template
           (RH,
            "{} is begin "
            & "return {}; "
            & "end {};",
            (1 => LALRW.Clone (LALRW.Handle (Fun.F_Subp_Spec)),
             2 => LALRW.Clone (LALRW.Handle (Fun.F_Expr)),
             3 => LALRW.Clone (LALRW.Handle (Fun.F_Subp_Spec.F_Subp_Name))),
            LALCO.Basic_Decl_Rule));
   end Handle_Expr_Function;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Expr_Function =>
            Handle_Expr_Function (Node.As_Expr_Function);
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
end Unsugar_Expr_Functions;
