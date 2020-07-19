with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

procedure Main is
   package Slocs   renames Langkit_Support.Slocs;

   package Helpers renames Libadalang.Helpers;
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;
   package LALU    renames Libadalang.Unparsing;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   package App is new Helpers.App
     (Name         => "AGC",
      Description  => "Garbage collection for Ada",
      Process_Unit => Process_Unit);

   procedure Handle_Allocator (Node : LAL.Allocator'Class)
   is
      Ctx : LAL.Analysis_Context := Node.Unit.Context;
      RH  : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Ctx);
      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);
   begin
      LALRW.Replace
        (SH, LALRW.Create_Call_Expr
           (RH,
            LALRW.Create_Dotted_Name
              (RH,
               LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, "GC"),
               LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, "Register")),
            LALRW.Clone (SH)));
      if not LALRW.Apply (RH).Success then
         raise Program_Error with "Could not rewrite unit";
      end if;
   end Handle_Allocator;

   procedure Add_With_Clause (Unit : LAL.Compilation_Unit'Class)
   is
      Ctx : LAL.Analysis_Context := Unit.Unit.Context;
      RH  : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Ctx);
      PH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Unit.F_Prelude);
   begin
      LALRW.Insert_Child
        (PH, 1,
         LALRW.Create_With_Clause
           (RH,
            LALRW.Create_Node (RH, LALCO.Ada_Limited_Absent),
            LALRW.Create_Node (RH, LALCO.Ada_Private_Absent),
            LALRW.Create_Regular_Node
              (RH, LALCO.Ada_Name_List,
               (1 => LALRW.Create_Token_Node
                  (RH, LALCO.Ada_Identifier, "GC")))));
      if not LALRW.Apply (RH).Success then
         raise Program_Error with "Could not add with clause";
      end if;
   end Add_With_Clause;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
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

      use type LALCO.Ada_Node_Kind_Type;
   begin
      if Unit.Root.Kind /= LALCO.Ada_Compilation_Unit then
         raise Program_Error with "Unhandled multi compilation unit files";
      end if;

      Add_With_Clause (Unit.Root.As_Compilation_Unit);

      if Unit.Has_Diagnostics then
         Put_Line ("Invalid ada unit " & Unit.Get_Filename);
      else
         Unit.Root.Traverse (Process_Node'Access);
         Put_Line (LALU.Unparse (Unit.Root));
      end if;
   end Process_Unit;
begin
   App.Run;
end Main;
