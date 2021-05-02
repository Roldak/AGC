with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;

with Analysis;
with Utils;

procedure Collect_Static
  (Job_Ctx  : Libadalang.Helpers.App_Job_Context;
   Unit     : Libadalang.Analysis.Analysis_Unit;
   Optimize : Boolean)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   --  Rewriting handle is created lazily in this phase because many units
   --  won't need to be rewritten.
   Internal_RH : LALRW.Rewriting_Handle := LALRW.No_Rewriting_Handle;

   function RH return LALRW.Rewriting_Handle is
      use type LALRW.Rewriting_Handle;
   begin
      if Internal_RH = LALRW.No_Rewriting_Handle then
         Internal_RH := LALRW.Start_Rewriting (Unit.Context);
      end if;
      return Internal_RH;
   end RH;

   function Apply_Rewritings_If_Relevant return Boolean is
      use type LALRW.Rewriting_Handle;
   begin
      if Internal_RH /= LALRW.No_Rewriting_Handle then
         return LALRW.Apply (Internal_RH).Success;
      end if;
      return True;
   end Apply_Rewritings_If_Relevant;

   procedure Handle_Handled_Stmts (Node : LAL.Handled_Stmts) is
      Decl_Part : LAL.Ada_Node := Node.Previous_Sibling;
      Last_Stmt : LAL.Ada_Node :=
         Node.F_Stmts.Child (Node.F_Stmts.Last_Child_Index);
   begin
      if Decl_Part.Is_Null then
         return;
      elsif Decl_Part.Kind not in LALCO.Ada_Declarative_Part then
         return;
      end if;

      for Decl of Decl_Part.As_Declarative_Part.F_Decls loop
         if Decl.Kind in LALCO.Ada_Object_Decl then
            for D of Decl.As_Object_Decl.F_Ids loop
               if Analysis.Is_Owner_After (D.As_Defining_Name, Last_Stmt) then
                  LALRW.Insert_Child
                    (LALRW.Handle (Node.F_Stmts),
                     Utils.Child_Index (LALRW.Handle (Last_Stmt)) + 1,
                     LALRW.Create_From_Template
                       (RH, "AGC.Free (" & D.Text & ");",
                        (1 .. 0 => <>), LALCO.Stmt_Rule));
               end if;
            end loop;
         end if;
      end loop;
   end Handle_Handled_Stmts;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Handled_Stmts =>
            Handle_Handled_Stmts (Node.As_Handled_Stmts);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   if not Optimize then
      return;
   end if;

   Unit.Root.Traverse (Process_Node'Access);
   if not Apply_Rewritings_If_Relevant then
      raise Program_Error with "collect_static: could not apply rewritings";
   end if;
end Collect_Static;
