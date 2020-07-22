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

procedure Handle_Temporaries
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   Alloc_Count_Map : Node_Counters.Counter;

   Alloc_Site_Count : Natural := 0;

   procedure Handle_Allocator
     (RH : LALRW.Rewriting_Handle; Node : LAL.Allocator'Class)
   is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);

      Scope : LAL.Ada_Node'Class := Utils.Find_Scope (Node);

      Alloc_Site : Langkit_Support.Text.Text_Type :=
         Alloc_Site_Count'Wide_Wide_Image;
   begin
      LALRW.Replace (SH, LALRW.Create_From_Template
        (RH, "GC.Temp (" & Alloc_Site & ", {})",
         (1 => LALRW.Clone (SH)), LALCO.Expr_Rule));

      Alloc_Site_Count := Alloc_Site_Count + 1;

      if Scope.Is_Null then
         raise Program_Error with "Could not find allocator's scope";
      end if;

      if Scope.Parent.Parent.Kind = LALCO.Ada_Declarative_Part then
         declare
            Stmts : LAL.Ada_Node :=
               Scope.Parent.Parent.Next_Sibling
                  .As_Handled_Stmts.F_Stmts.As_Ada_Node;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts);
         begin
            LALRW.Insert_Child (TH, 1, LALRW.Create_From_Template
              (RH, "GC.Untemp (" & Alloc_Site & ");",
               (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Node_Counters.Increase (Alloc_Count_Map, Stmts);
         end;
      else
         declare
            Stmts : LAL.Ada_Node := Scope.Parent;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts);

            Index : Positive :=
               LAL.Child_Index (Scope)
               + Node_Counters.Get (Alloc_Count_Map, Stmts)
               + 2;
         begin
            LALRW.Insert_Child (TH, Index,
               LALRW.Create_From_Template
                 (RH, "GC.Untemp (" & Alloc_Site & ");",
                  (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Node_Counters.Increase (Alloc_Count_Map, Stmts);
         end;
      end if;
   end Handle_Allocator;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Allocator =>
            Handle_Allocator (RH, Node.As_Allocator);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
   if not LALRW.Apply (RH).Success then
      raise Program_Error with "handle_temporaries: could not apply rewritings";
   end if;
end Handle_Temporaries;
