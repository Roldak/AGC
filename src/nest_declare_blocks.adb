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

procedure Nest_Declare_Blocks
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Populate_Block
     (Decls_Block  : LALRW.Node_Rewriting_Handle;
      Stmts_Block  : LALRW.Node_Rewriting_Handle;
      Decl_Content : LAL.Ada_Node_List;
      Stmt_Content : LALRW.Node_Rewriting_Handle;
      Decl_Index   : Natural)
   is
      Index : Natural := Decl_Index;
   begin
      while Index <= Decl_Content.Last_Child_Index loop
         declare
            Child : LAL.Ada_Node := LAL.Child (Decl_Content, Index);
         begin
            LALRW.Replace
              (LALRW.Handle (Child), LALRW.No_Node_Rewriting_Handle);
            LALRW.Append_Child (Decls_Block, LALRW.Handle (Child));
            Index := Index + 1;
            if Child.Kind in LALCO.Ada_Object_Decl then
               declare
                  New_Decls : LALRW.Node_Rewriting_Handle :=
                     LALRW.Create_Regular_Node
                       (RH, LALCO.Ada_Ada_Node_List, (1 .. 0 => <>));

                  New_Decl_Part : LALRW.Node_Rewriting_Handle :=
                     LALRW.Create_Declarative_Part (RH, New_Decls);

                  New_Stmts : LALRW.Node_Rewriting_Handle :=
                     LALRW.Create_Regular_Node
                       (RH, LALCO.Ada_Stmt_List, (1 .. 0 => <>));

                  New_Handled_Stmts : LALRW.Node_Rewriting_Handle :=
                     LALRW.Create_Handled_Stmts
                       (RH, New_Stmts, LALRW.Create_Regular_Node
                          (RH, LALCO.Ada_Ada_Node_List, (1 .. 0 => <>)));
               begin
                  LALRW.Append_Child (Stmts_Block, LALRW.Create_Decl_Block
                    (RH,
                     New_Decl_Part,
                     New_Handled_Stmts,
                     LALRW.No_Node_Rewriting_Handle));

                  Populate_Block
                    (New_Decls,
                     New_Stmts,
                     Decl_Content,
                     Stmt_Content,
                     Index);
               end;
               return;
            end if;
         end;
      end loop;
      LALRW.Replace (Stmts_Block, Stmt_Content);
   end Populate_Block;

   procedure Handle_Handled_Stmts (Node : LAL.Handled_Stmts'Class) is
      use type LALCO.Ada_Node_Kind_Type;

      Sibling : LAL.Ada_Node := LAL.Previous_Sibling (Node);

      SH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node.F_Stmts);
   begin
      if Sibling.Kind = LALCO.Ada_Declarative_Part then
         declare
            Decl_Part : LAL.Declarative_Part := Sibling.As_Declarative_Part;
            Decls     : LAL.Ada_Node_List := Decl_Part.F_Decls;

            DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decls);

            New_Decls : LALRW.Node_Rewriting_Handle :=
               LALRW.Create_Regular_Node
                 (RH, LALCO.Ada_Ada_Node_List, (1 .. 0 => <>));

            New_Stmts : LALRW.Node_Rewriting_Handle :=
               LALRW.Create_Regular_Node
                 (RH, LALCO.Ada_Stmt_List, (1 .. 0 => <>));
         begin
            LALRW.Replace (DH, New_Decls);
            LALRW.Replace (SH, New_Stmts);
            Populate_Block
              (New_Decls,
               New_Stmts,
               Decls,
               SH,
               Decls.First_Child_Index);
         end;
      end if;
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
   Unit.Root.Traverse (Process_Node'Access);
   if not LALRW.Apply (RH).Success then
      raise Program_Error with "nest_declare_blocks: could not apply rewritings";
   end if;
end Nest_Declare_Blocks;
