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

   Temp_Count_Map  : Node_Counters.Counter;
   Temp_Site_Count : Natural := 0;

   procedure Handle_Expr (Expr : LAL.Expr'Class) is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Expr);

      Scope : LAL.Ada_Node'Class := Utils.Find_Scope (Expr);

      Temp_Site_Id : Natural := Node_Counters.Get_Or_Set
        (Temp_Count_Map, Scope.As_Ada_Node, Temp_Site_Count);

      Temp_Site : Langkit_Support.Text.Text_Type :=
         Temp_Site_Id'Wide_Wide_Image;
   begin
      LALRW.Replace (SH, LALRW.Create_From_Template
        (RH, "GC.Temp (" & Temp_Site & ", {})",
         (1 => LALRW.Clone (SH)), LALCO.Expr_Rule));

      if Temp_Site_Id /= Temp_Site_Count then
         --  Call to GC.Untemp already added by a previous iteration
         return;
      end if;

      Temp_Site_Count := Temp_Site_Count + 1;

      if Scope.Is_Null then
         raise Program_Error with "Could not find allocator's scope";
      end if;

      if Scope.Kind = LALCO.Ada_Extended_Return_Stmt then
         declare
            Stmts : LAL.Ada_Node :=
               Scope.As_Extended_Return_Stmt.F_Stmts.F_Stmts.As_Ada_Node;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts);
         begin
            if
               Stmts.Children_Count = 1
               and then Stmts.Child (1).Kind = LALCO.Ada_Null_Stmt
            then
               LALRW.Remove_Child (TH, 1);
            end if;
            LALRW.Insert_Child (TH, 1, LALRW.Create_From_Template
              (RH, "GC.Untemp (" & Temp_Site & ");",
               (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Node_Counters.Increase (Alloc_Count_Map, Stmts);
         end;
      elsif Scope.Parent.Parent.Kind = LALCO.Ada_Declarative_Part then
         declare
            Stmts : LAL.Ada_Node :=
               Scope.Parent.Parent.Next_Sibling
                  .As_Handled_Stmts.F_Stmts.As_Ada_Node;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts);
         begin
            LALRW.Insert_Child (TH, 1, LALRW.Create_From_Template
              (RH, "GC.Untemp (" & Temp_Site & ");",
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
                 (RH, "GC.Untemp (" & Temp_Site & ");",
                  (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Node_Counters.Increase (Alloc_Count_Map, Stmts);
         end;
      end if;
   end Handle_Expr;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Expr =>
            declare
               Expr      : LAL.Expr := Node.As_Expr;
               Expr_Type : LAL.Base_Type_Decl :=
                  Expr.P_Expression_Type;
            begin
               if
                  not Expr_Type.Is_Null
                  and then Utils.Is_Relevant_Type (Expr_Type)
                  and then not Utils.Is_Named_Expr (Expr)
               then
                  Handle_Expr (Expr);
               end if;
            exception
               when LALCO.Property_Error =>
                  null;
            end;
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
