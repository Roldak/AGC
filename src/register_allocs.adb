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

   package Node_Counters is new Ada.Containers.Hashed_Maps
     (LAL.Ada_Node, Natural, LAL.Hash, LAL."=");
   use type Node_Counters.Cursor;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   Alloc_Count_Map : Node_Counters.Map;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class
   is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      Parent       : LAL.Ada_Node := N.Parent;
      Grand_Parent : LAL.Ada_Node :=
        (if Parent.Is_Null then LAL.No_Ada_Node else Parent.Parent);
   begin
      if Grand_Parent.Is_Null then
         return LAL.No_Ada_Node;
      elsif Grand_Parent.Kind = LALCO.Ada_Declarative_Part then
         return N;
      elsif Parent.Kind = LALCO.Ada_Stmt_List then
         return N;
      else
         return Find_Scope (Parent);
      end if;
   end Find_Scope;

   procedure Increase_Node_Counter (Node : LAL.Ada_Node) is
      Cursor : Node_Counters.Cursor := Node_Counters.Find
        (Alloc_Count_Map, Node);
   begin
      if Cursor = Node_Counters.No_element then
         Node_Counters.Insert
           (Alloc_Count_Map, Node, 1);
      else
         Node_Counters.Replace_Element
           (Alloc_Count_Map, Cursor, Node_Counters.Element (Cursor) + 1);
      end if;
   end Increase_Node_Counter;

   function Get_Node_Counter (Node : LAL.Ada_Node) return Natural is
      Cursor : Node_Counters.Cursor := Node_Counters.Find
        (Alloc_Count_Map, Node);
   begin
      if Cursor = Node_Counters.No_element then
         return 0;
      else
         return Node_Counters.Element (Cursor);
      end if;
   end Get_Node_Counter;

   Alloc_Site_Count : Natural := 0;

   procedure Handle_Allocator
     (RH : LALRW.Rewriting_Handle; Node : LAL.Allocator'Class)
   is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);

      Scope : LAL.Ada_Node'Class := Find_Scope (Node);

      Alloc_Site : Langkit_Support.Text.Text_Type :=
         Alloc_Site_Count'Wide_Wide_Image;
   begin
      LALRW.Replace (SH, LALRW.Create_From_Template
        (RH, "GC.Register (" & Alloc_Site & ", {})",
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

            Increase_Node_Counter (Stmts);
         end;
      else
         declare
            Stmts : LAL.Ada_Node := Scope.Parent;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts);

            Index : Positive :=
               LAL.Child_Index (Scope) + Get_Node_Counter (Stmts) + 2;
         begin
            LALRW.Insert_Child (TH, Index,
               LALRW.Create_From_Template
                 (RH, "GC.Untemp (" & Alloc_Site & ");",
                  (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Increase_Node_Counter (Stmts);
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
      raise Program_Error with "register_allocs: could not apply rewritings";
   end if;
end Register_Allocs;
