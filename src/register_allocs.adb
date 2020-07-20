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

   function Hash (X : LAL.Handled_Stmts) return Ada.Containers.Hash_Type is
   begin
      return LAL.Hash (X.As_Ada_Node);
   end Hash;

   package Alloc_Maps is new Ada.Containers.Hashed_Maps
     (LAL.Handled_Stmts, Natural, Hash, LAL."=");
   use type Alloc_Maps.Cursor;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   Alloc_Map : Alloc_Maps.Map;

   function Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class
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
      elsif Grand_Parent.Kind = LALCO.Ada_Handled_Stmts then
         return N;
      else
         return Scope (Parent);
      end if;
   end Scope;

   procedure Update_Stmt_Counter (Stmts : LAL.Handled_Stmts) is
      Cursor : Alloc_Maps.Cursor := Alloc_Maps.Find
        (Alloc_Map, Stmts);
   begin
      if Cursor = Alloc_Maps.No_element then
         Alloc_Maps.Insert
           (Alloc_Map, Stmts, 1);
      else
         Alloc_Maps.Replace_Element
           (Alloc_Map, Cursor, Alloc_Maps.Element (Cursor) + 1);
      end if;
   end Update_Stmt_Counter;

   function Get_Stmt_Counter (Stmts : LAL.Handled_Stmts) return Natural is
      Cursor : Alloc_Maps.Cursor := Alloc_Maps.Find
        (Alloc_Map, Stmts);
   begin
      if Cursor = Alloc_Maps.No_element then
         return 0;
      else
         return Alloc_Maps.Element (Cursor);
      end if;
   end Get_Stmt_Counter;

   Alloc_Site_Count : Natural := 0;

   procedure Handle_Allocator
     (RH : LALRW.Rewriting_Handle; Node : LAL.Allocator'Class)
   is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);

      List : LAL.Ada_Node'Class := Scope (Node);

      Alloc_Site : Langkit_Support.Text.Text_Type :=
         Alloc_Site_Count'Wide_Wide_Image;
   begin
      LALRW.Replace (SH, LALRW.Create_From_Template
        (RH, "GC.Register (" & Alloc_Site & ", {})",
         (1 => LALRW.Clone (SH)), LALCO.Expr_Rule));

      Alloc_Site_Count := Alloc_Site_Count + 1;

      if List.Is_Null then
         raise Program_Error with "Could not find allocator's scope";
      end if;

      if List.Parent.Parent.Kind = LALCO.Ada_Declarative_Part then
         declare
            Stmts : LAL.Handled_Stmts :=
               List.Parent.Parent.Next_Sibling.As_Handled_Stmts;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts.F_Stmts);
         begin
            LALRW.Insert_Child (TH, 1, LALRW.Create_From_Template
              (RH, "GC.Untemp (" & Alloc_Site & ");",
               (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Update_Stmt_Counter (Stmts);
         end;
      else
         declare
            Stmts : LAL.Handled_Stmts :=
               List.Parent.Parent.As_Handled_Stmts;

            TH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Stmts.F_Stmts);

            Index : Positive :=
               LAL.Child_Index (List) + Get_Stmt_Counter (Stmts) + 2;
         begin
            LALRW.Insert_Child (TH, Index,
               LALRW.Create_From_Template
                 (RH, "GC.Untemp (" & Alloc_Site & ");",
                  (1 .. 0 => <>), LALCO.Call_Stmt_Rule));

            Update_Stmt_Counter (Stmts);
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
