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

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (LAL.Ada_Node,
      LALRW.Node_Rewriting_Handle,
      LAL.Hash,
      LAL."=",
      LALRW."=");

   Decl_Blocks : Node_Maps.Map;
   Temp_Site   : Node_Counters.Counter;

   function Get_Or_Create_Decl_Block
     (Scope : LAL.Ada_Node) return LALRW.Node_Rewriting_Handle
   is
      use type Node_Maps.Cursor;

      Cursor : Node_Maps.Cursor := Node_Maps.Find (Decl_Blocks, Scope);
      Block  : LALRW.Node_Rewriting_Handle;
   begin
      if Cursor = Node_Maps.No_Element then
         Block := LALRW.Create_From_Template
           (RH,
            "declare begin end;",
            (1 .. 0 => <>),
            LALCO.Block_Stmt_Rule);

         LALRW.Replace (LALRW.Handle (Scope), Block);
         LALRW.Append_Child
           (LALRW.Child (LALRW.Child (Block, 2), 1),
            LALRW.Handle (Scope));

         Node_Maps.Insert (Decl_Blocks, Scope, Block);
      else
         Block := Node_Maps.Element (Cursor);
      end if;

      return LALRW.Child (LALRW.Child (Block, 1), 1);
   end Get_Or_Create_Decl_Block;

   procedure Handle_Expr (Expr : LAL.Expr'Class) is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      EH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Expr);

      Scope : LAL.Ada_Node'Class := Utils.Find_Scope (Expr);
   begin
      if Scope.Is_Null then
         raise Program_Error with "Could not find expr's scope";
      end if;

      if Scope.Parent.Kind = LALCO.Ada_Stmt_List then
         declare
            Temp_Id   : Langkit_Support.Text.Text_Type :=
               Node_Counters.Get
                 (Temp_Site, Scope.As_Ada_Node)'Wide_Wide_Image;

            Temp_Name : Langkit_Support.Text.Text_Type :=
               "AGC_Temp_" & Temp_Id (Temp_Id'First + 1 .. Temp_Id'Last);

            Decls : LALRW.Node_Rewriting_Handle :=
               Get_Or_Create_Decl_Block (Scope.As_Ada_Node);

            Obj_Decl : LALRW.Node_Rewriting_Handle :=
               LALRW.Create_From_Template
                 (RH,
                  Temp_Name & " : {} := null;",
                  (1 => Utils.Generate_Type_Reference
                          (RH, Expr.P_Expression_Type)),
                  LALCO.Object_Decl_Rule);
         begin
            LALRW.Replace (EH, LALRW.Create_From_Template
              (RH, Temp_Name, (1 .. 0 => <>), LALCO.Identifier_Rule));

            LALRW.Set_Child (Obj_Decl, 6, EH);

            LALRW.Insert_Child (Decls, 1, Obj_Decl);

            Node_Counters.Increase (Temp_Site, Scope.As_Ada_Node);
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
               Expr_Type : LAL.Base_Type_Decl;
            begin
               Expr_Type := Expr.P_Expression_Type;
               if
                  not Expr_Type.Is_Null
                  and then Utils.Is_Relevant_Type (Expr_Type)
                  and then Utils.Is_Actual_Expr (Expr)
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
