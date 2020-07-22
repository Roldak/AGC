with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

with Utils;

procedure Track_Roots
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Push_Object
     (Stmts : LALRW.Node_Rewriting_Handle; X : LAL.Object_Decl)
   is
      Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (X.P_Defining_Name);
   begin
      LALRW.Insert_Child
        (Stmts, 1,
         LALRW.Create_From_Template
           (RH,
            "GC.Push_Root ({}'Address);",
            (1 => LALRW.Create_Token_Node
                    (RH, LALCO.Ada_Identifier, Name)),
            LALCO.Call_Stmt_Rule));
   end Push_Object;

   procedure Pop_Objects (Stmts : LALRW.Node_Rewriting_Handle) is
   begin
      LALRW.Append_Child
        (Stmts,
         LALRW.Create_From_Template
           (RH,
            "GC.Pop_Roots (AGC_Root_Count);",
            (1 .. 0 => <>),
            LALCO.Call_Stmt_Rule));
   end Pop_Objects;

   procedure Handle_Aliased_Annot (Node : LAL.Aliased_Absent'Class)
   is
      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);
   begin
      if Node.Parent.Kind in LALCO.Ada_Object_Decl then
         if Utils.Is_Relevant_Root (Node.Parent.As_Basic_Decl) then
            LALRW.Replace
              (SH, LALRW.Create_Node (RH, LALCO.Ada_Aliased_Present));
         end if;
      end if;
   end Handle_Aliased_Annot;

   procedure Handle_Handled_Stmts (Node : LAL.Handled_Stmts'Class)
   is
      use type LALCO.Ada_Node_Kind_Type;

      Sibling : LAL.Ada_Node := LAL.Previous_Sibling (Node);

      SH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node.F_Stmts);
   begin
      if Sibling.Kind = LALCO.Ada_Declarative_Part then
         declare
            Decl_Part : LAL.Declarative_Part := Sibling.As_Declarative_Part;
            Decls     : LAL.Ada_Node_List := Decl_Part.F_Decls;

            DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decls);
         begin
            LALRW.Insert_Child (DH, 1, LALRW.Create_From_Template
              (RH,
               "AGC_Root_Count : Natural := GC.Root_Count;",
               (1 .. 0 => <>),
               LALCO.Object_Decl_Rule));

            for N in Decls.First_Child_Index .. Decls.Last_Child_Index loop
               declare
                  C : LAL.Ada_Node := LAL.Child (Decls, N);
               begin
                  if C.Kind = LALCO.Ada_Object_Decl then
                     if Utils.Is_Relevant_Root (C.As_Basic_Decl) then
                        Push_Object (SH, C.As_Object_Decl);
                     end if;
                  end if;
               end;
            end loop;
            Pop_Objects (SH);
         end;
      end if;
   end Handle_Handled_Stmts;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Aliased_Absent =>
            Handle_Aliased_Annot (Node.As_Aliased_Absent);
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
      raise Program_Error with "track_roots: could not apply rewritings";
   end if;
end Track_Roots;
