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
      Id  : LALRW.Node_Rewriting_Handle := LALRW.Create_Token_Node
        (RH, LALCO.Ada_Identifier, "GC_Register");
      CE  : LALRW.Node_Rewriting_Handle := LALRW.Create_Call_Expr
        (RH, Id, LALRW.Clone (SH));
   begin
      LALRW.Replace (SH, CE);
      if not LALRW.Apply (RH).Success then
         raise Program_Error with "Could not rewrite unit";
      end if;
   end Handle_Allocator;

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
   begin
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
