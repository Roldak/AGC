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

procedure Pass.Unsugar_Expr_Functions
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit    : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Handle_Expr_Function (Fun : LAL.Expr_Function'Class) is
      Has_Decl : Boolean  := not Fun.P_Previous_Part.Is_Null;

      Actual_Expr : LAL.Expr := Fun.F_Expr;

      New_Fun : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
        (RH,
         "{} is begin return {}; end {};",
         (1 => LALRW.Clone (LALRW.Handle (Fun.F_Subp_Spec)),
          2 => LALRW.Clone (LALRW.Handle (Actual_Expr)),
          3 => LALRW.Clone (LALRW.Handle (Fun.F_Subp_Spec.F_Subp_Name))),
         LALCO.Basic_Decl_Rule);

      Index : Natural := Utils.Child_Index (LALRW.Handle (Fun));
   begin
      if Has_Decl then
         LALRW.Replace (LALRW.Handle (Fun), New_Fun);
      else
         LALRW.Replace
           (LALRW.Handle (Fun),
            LALRW.Create_From_Template
              (RH, "{};",
               (1 => LALRW.Clone (LALRW.Handle (Fun.F_Subp_Spec))),
               LALCO.Basic_Decl_Rule));
         LALRW.Insert_Child
           (LALRW.Handle (Fun.Parent), Index + 1, New_Fun);
      end if;
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
end Pass.Unsugar_Expr_Functions;
