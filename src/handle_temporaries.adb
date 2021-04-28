with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

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

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   Decl_Blocks  : Node_Maps.Map;
   Temp_Site    : Node_Counters.Counter;
   Decl_Site    : Node_Counters.Counter;
   Dirty_Scopes : Node_Sets.Set;

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

   function Get_Or_Create_Function
     (Scope : LAL.Object_Decl) return LALRW.Node_Rewriting_Handle
   is
      use type Node_Maps.Cursor;

      Node   : LAL.Ada_Node := Scope.As_Ada_Node;
      Cursor : Node_Maps.Cursor := Node_Maps.Find (Decl_Blocks, Node);
      Func   : LALRW.Node_Rewriting_Handle;

      DH  : LALRW.Node_Rewriting_Handle :=
         LALRW.Handle (Scope.F_Default_Expr);

      Func_Id   : Langkit_Support.Text.Text_Type :=
         Node_Counters.Get (Decl_Site, Node.Parent)'Wide_Wide_Image;

      Func_Name : Langkit_Support.Text.Text_Type :=
         "AGC_Func_" & Func_Id (Func_Id'First + 1 .. Func_Id'Last);

      Child_Index : Integer :=
         Scope.Child_Index
         + Node_Counters.Get (Decl_Site, Node.Parent) * 2
         + 1;

      Ret_Type_Expr : LALRW.Node_Rewriting_Handle;
   begin
      if Cursor = Node_Maps.No_Element then
         Ret_Type_Expr := LALRW.Clone (LALRW.Handle (Scope.F_Type_Expr));

         if Scope.F_Type_Expr.Kind in LALCO.Ada_Subtype_Indication then
            --  Remove constraint from subtype indication in return type
            LALRW.Set_Child
              (Ret_Type_Expr, 3, LALRW.No_Node_Rewriting_Handle);
         end if;

         LALRW.Insert_Child
           (LALRW.Handle (Scope.Parent),
            Child_Index,
            LALRW.Create_From_Template
              (RH,
               "function " & Func_Name & " return {};",
               (1 => Ret_Type_Expr),
               LALCO.Subp_Decl_Rule));

         Func := LALRW.Create_From_Template
           (RH,
            "function " & Func_Name & " return {} "
            & "is begin return AGC_Ret : X := null do "
            & "null; end return; end " & Func_Name & ";",
            (1 => Ret_Type_Expr),
            LALCO.Subp_Body_Rule);

         LALRW.Replace
           (LALRW.Handle (Scope.F_Type_Expr),
            Ret_Type_Expr);
         LALRW.Replace
           (DH,
            LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, Func_Name));
         LALRW.Set_Child
           (LALRW.Child
              (LALRW.Child (LALRW.Child (LALRW.Child (Func, 5), 1), 1), 1),
            5,
            LALRW.Handle (Scope.F_Type_Expr));
         LALRW.Set_Child
           (LALRW.Child
              (LALRW.Child (LALRW.Child (LALRW.Child (Func, 5), 1), 1), 1),
            6,
            DH);
         LALRW.Insert_Child
           (LALRW.Handle (Scope.Parent), Child_Index + 1, Func);

         Node_Maps.Insert (Decl_Blocks, Node, Func);
         Node_Counters.Increase (Decl_Site, Node.Parent);
      else
         Func := Node_Maps.Element (Cursor);
      end if;

      return LALRW.Child (LALRW.Child (Func, 4), 1);
   end Get_Or_Create_Function;

   function Expr_Type_Name
     (Expr : LAL.Expr'Class) return Langkit_Support.Text.Text_Type
   is
      Expr_Type : LAL.Base_Type_Decl'Class := Expr.P_Expression_Type;
   begin
      return Utils.Generate_Type_Reference (Expr_Type, Unit)
         & (if not Expr_Type.P_Is_Classwide and then
               Expr_Type.P_Is_Tagged_Type and then
               Expr.P_Is_Dynamically_Tagged
            then "'Class"
            else "");
   end Expr_Type_Name;

   function Get_Or_Create_Function
     (Expr      : LAL.Expr'Class;
      Fun_Scope : LALRW.Node_Rewriting_Handle)
      return LALRW.Node_Rewriting_Handle
   is
      use type Node_Maps.Cursor;

      Node   : LAL.Ada_Node := Expr.As_Ada_Node;
      Cursor : Node_Maps.Cursor := Node_Maps.Find (Decl_Blocks, Node);
      Func   : LALRW.Node_Rewriting_Handle;

      Func_Id   : Langkit_Support.Text.Text_Type :=
         Node_Counters.Get (Decl_Site, LALRW.Node (Fun_Scope))'Wide_Wide_Image;

      Func_Name : Langkit_Support.Text.Text_Type :=
         "AGC_Func_" & Func_Id (Func_Id'First + 1 .. Func_Id'Last);

      Type_Name : Langkit_Support.Text.Text_Type := Expr_Type_Name (Expr);
   begin
      if Cursor = Node_Maps.No_Element then
         Func := LALRW.Create_From_Template
           (RH,
            "function " & Func_Name & " return "
            & Type_Name & " is begin return AGC_Ret : "
            & Type_Name & " := null do null; end return; end "
            & Func_Name & ";",
            (1 .. 0 => <>),
            LALCO.Subp_Body_Rule);

         LALRW.Replace
           (LALRW.Handle (Expr),
            LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, Func_Name));
         LALRW.Set_Child
           (LALRW.Child
              (LALRW.Child (LALRW.Child (LALRW.Child (Func, 5), 1), 1), 1),
            6,
            LALRW.Handle (Expr));
         LALRW.Insert_Child (Fun_Scope, 1, Func);

         Node_Maps.Insert (Decl_Blocks, Node, Func);
         Node_Counters.Increase (Decl_Site, LALRW.Node (Fun_Scope));
      else
         Func := Node_Maps.Element (Cursor);
      end if;

      return LALRW.Child (LALRW.Child (Func, 4), 1);
   end Get_Or_Create_Function;

   function Find_Scope_Or_Raise
     (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class
   is
   begin
      return Res : LAL.Ada_Node'Class := Utils.Find_Scope (N) do
         if Res.Is_Null then
            raise Program_Error with "Could not find expr's scope";
         end if;
      end return;
   end Find_Scope_Or_Raise;

   function Decl_Block_For
     (Scope : LAL.Ada_Node'Class) return LALRW.Node_Rewriting_Handle
   is
   begin
      case Scope.Kind is
         when LALCO.Ada_Object_Decl =>
            return Get_Or_Create_Function (Scope.As_Object_Decl);
         when LALCO.Ada_Expr =>
            return Get_Or_Create_Function
              (Scope.As_Expr,
               Decl_Block_For (Find_Scope_Or_Raise (Scope.Parent)));
         when others =>
            return Get_Or_Create_Decl_Block (Scope.As_Ada_Node);
      end case;
   end Decl_Block_For;

   procedure Handle_Expr (Expr : LAL.Expr'Class) is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      Scope : LAL.Ada_Node'Class := Find_Scope_Or_Raise (Expr);

      EH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Expr);

      Temp_Id   : Langkit_Support.Text.Text_Type :=
         Node_Counters.Get
           (Temp_Site, Scope.As_Ada_Node)'Wide_Wide_Image;

      Temp_Name : Langkit_Support.Text.Text_Type :=
         "AGC_Temp_" & Temp_Id (Temp_Id'First + 1 .. Temp_Id'Last);

      Decls : LALRW.Node_Rewriting_Handle := Decl_Block_For (Scope);

      Obj_Decl : LALRW.Node_Rewriting_Handle :=
         LALRW.Create_From_Template
           (RH,
            Temp_Name & " : "
            & Expr_Type_Name (Expr)
            & " := null;",
            (1 .. 0 => <>),
            LALCO.Object_Decl_Rule);

      Needs_Parentheses : Boolean := Expr.Kind
         in LALCO.Ada_Raise_Expr
          | LALCO.Ada_Cond_Expr;
   begin
      LALRW.Replace (EH, LALRW.Create_From_Template
        (RH, Temp_Name, (1 .. 0 => <>), LALCO.Identifier_Rule));

      LALRW.Set_Child
        (Obj_Decl, 6,
         (if Needs_Parentheses
          then LALRW.Create_Paren_Expr (RH, EH)
          else EH));

      LALRW.Insert_Child (Decls, 1, Obj_Decl);

      Node_Counters.Increase (Temp_Site, Scope.As_Ada_Node);
   end Handle_Expr;

   procedure Flag_Dirty (Expr : LAL.Expr'Class) is
      Scope : LAL.Ada_Node'Class := Find_Scope_Or_Raise (Expr);
   begin
      Dirty_Scopes.Include (Scope.As_Ada_Node);
   end Flag_Dirty;

   function Is_Dirty (Expr : LAL.Expr'Class) return Boolean is
      Scope : LAL.Ada_Node'Class := Find_Scope_Or_Raise (Expr);
   begin
      return Dirty_Scopes.Contains (Scope.As_Ada_Node);
   end Is_Dirty;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
      use type LAL.Expr;
   begin
      case Node.Kind is
         when LALCO.Ada_Aspect_Spec =>
            return LALCO.Over;
         when LALCO.Ada_Pragma_Node =>
            return LALCO.Over;
         when LALCO.Ada_Expr =>
            declare
               Expr : LAL.Expr := Node.As_Expr;
            begin
               if
                  Utils.Is_Actual_Expr (Expr)
                  and then Utils.Is_Relevant_Type (Expr.P_Expression_Type)
                  and then not Utils.Is_Named_Expr (Expr)
               then
                  if Utils.Expands_To_Loop (Expr) then
                     Utils.Output_Diagnostic
                       (Expr.Parent,
                        "expression expanding to loop not yet handled",
                        Utils.Warning);
                  end if;

                  if Expr.Parent.Kind in
                       LALCO.Ada_Paren_Expr
                       | LALCO.Ada_Object_Decl
                       | LALCO.Ada_Component_Decl
                       | LALCO.Ada_Assign_Stmt
                       | LALCO.Ada_Return_Stmt
                       | LALCO.Ada_Extended_Return_Stmt
                       | LALCO.Ada_Renaming_Clause
                       | LALCO.Ada_Case_Expr_Alternative
                       | LALCO.Ada_Elsif_Expr_Part
                     or else (Expr.Parent.Kind in LALCO.Ada_If_Expr and then
                              Expr.Parent.As_If_Expr.F_Cond_Expr /= Expr)
                  then
                     Flag_Dirty (Expr);
                  elsif (Expr.Parent.Kind in LALCO.Ada_Basic_Assoc
                         and then not Is_Dirty (Expr))
                  then
                     Flag_Dirty (Expr);
                  else
                     Handle_Expr (Expr);
                  end if;
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
