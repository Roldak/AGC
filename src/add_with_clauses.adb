with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

procedure Add_With_Clauses
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

   declare
      WH : LALRW.Node_Rewriting_Handle :=
         LALRW.Handle (Unit.Root.As_Compilation_Unit.F_Prelude);

      Decl_Part : LAL.Declarative_Part :=
         Unit.Root.As_Compilation_Unit.P_Decl.P_Declarative_Region;
   begin
      LALRW.Insert_Child (WH, 1, LALRW.Create_From_Template
        (RH, "with AGC;", (1 .. 0 => <>), LALCO.With_Clause_Rule));
      LALRW.Insert_Child (WH, 2, LALRW.Create_From_Template
        (RH, "with AGC.Storage;", (1 .. 0 => <>), LALCO.With_Clause_Rule));
      LALRW.Insert_Child (WH, 3, LALRW.Create_From_Template
        (RH, "with System;", (1 .. 0 => <>), LALCO.With_Clause_Rule));
      LALRW.Insert_Child (WH, 4, LALRW.Create_From_Template
        (RH, "with Ada.Unchecked_Conversion;",
         (1 .. 0 => <>), LALCO.With_Clause_Rule));

      if not Decl_Part.Is_Null then
         LALRW.Insert_Child
           (LALRW.Handle (Decl_Part.F_Decls), 1,
            LALRW.Create_From_Template
              (RH,
               "pragma Default_Storage_Pool (AGC.Storage.Pool);",
               (1 .. 0 => <>),
               LALCO.Pragma_Rule));
      end if;
   end;

   if not LALRW.Apply (RH).Success then
      raise Program_Error with "add_with_clause: could not apply rewritings";
   end if;
end Add_With_Clauses;
