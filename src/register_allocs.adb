with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Hashed_Maps;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

procedure Register_Allocs
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Handle_Allocator (Node : LAL.Allocator'Class) is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);
   begin
      LALRW.Replace (SH, LALRW.Create_From_Template
        (RH, "AGC_Register ({})",
         (1 => LALRW.Clone (SH)), LALCO.Expr_Rule));
   end Handle_Allocator;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Allocator =>
            Handle_Allocator (Node.As_Allocator);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
   if not LALRW.Apply (RH).Success then
      raise Program_Error with "register_allocs: could not apply rewritings";
   end if;
end Register_Allocs;
