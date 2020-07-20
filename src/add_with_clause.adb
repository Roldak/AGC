with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

procedure Add_With_Clause
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   use type LALCO.Ada_Node_Kind_Type;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);
begin
   if Unit.Root.Kind /= LALCO.Ada_Compilation_Unit then
      raise Program_Error with "Unhandled multi compilation unit files";
   end if;

   LALRW.Insert_Child
     (LALRW.Handle (Unit.Root.As_Compilation_Unit.F_Prelude), 1,
      LALRW.Create_From_Template
        (RH, "with GC;", (1 .. 0 => <>), LALCO.With_Clause_Rule));

   if not LALRW.Apply (RH).Success then
      raise Program_Error with "add_with_clause: could not apply rewritings";
   end if;
end Add_With_Clause;
