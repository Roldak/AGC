with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

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

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   Handled_Stmts : Node_Sets.Set;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Push_Object
     (Stmts : LALRW.Node_Rewriting_Handle; X : LAL.Object_Decl)
   is
      Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (X.P_Defining_Name);

      Obj_Type : LAL.Base_Type_Decl'Class :=
         X.P_Type_Expression.P_Designated_Type_Decl;
   begin
      LALRW.Insert_Child
        (Stmts, 1,
         LALRW.Create_From_Template
           (RH,
            "GC.Push_Root ({}'Address, {}'Address);",
            (1 => LALRW.Create_Token_Node
                    (RH, LALCO.Ada_Identifier, Name),
             2 => LALRW.Create_Token_Node
                    (RH, LALCO.Ada_Identifier,
                     Utils.Visitor_Name (Obj_Type))),
            LALCO.Call_Stmt_Rule));
   end Push_Object;

   function Root_Count_Name
     (Subp_Level : Boolean) return Langkit_Support.Text.Text_Type
   is
     (if Subp_Level then "AGC_Base_Root_Count"
      else "AGC_Root_Count");

   procedure Pop_Objects
     (Stmts        : LALRW.Node_Rewriting_Handle;
      Leaving_Subp : Boolean)
   is
   begin
      if
         LALRW.Children_Count (Stmts) = 1
         and then LALRW.Kind (LALRW.Child (Stmts, 1)) in LALCO.Ada_Null_Stmt
      then
         LALRW.Remove_Child (Stmts, 1);
      end if;
      LALRW.Append_Child
        (Stmts,
         LALRW.Create_From_Template
           (RH,
            "GC.Pop_Roots (" & Root_Count_Name (Leaving_Subp) & ");",
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
            Decl_Part  : LAL.Declarative_Part := Sibling.As_Declarative_Part;
            Decls      : LAL.Ada_Node_List := Decl_Part.F_Decls;
            Container  : LAL.Ada_Node := Decl_Part.Parent;
            Subp_Level : Boolean :=
               Container.Kind in LALCO.Ada_Base_Subp_Body;

            DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decls);

            Should_Pop_Roots : Boolean :=
               not Handled_Stmts.Contains (Node.Parent.Parent.Parent)
               or else Node.Parent.Child_Index
                          /= Node.Parent.Parent.Children_Count - 1;
         begin
            Handled_Stmts.Insert (Node.As_Ada_Node);

            if Should_Pop_Roots then
               LALRW.Insert_Child (DH, 1, LALRW.Create_From_Template
                 (RH,
                  Root_Count_Name (Subp_Level)
                  & " : Natural := GC.Root_Count;",
                  (1 .. 0 => <>),
                  LALCO.Object_Decl_Rule));
            end if;

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

            if Should_Pop_Roots then
               Pop_Objects (SH, Subp_Level);
            end if;
         end;
      end if;
   end Handle_Handled_Stmts;

   procedure Handle_Return_Stmt
     (Stmt : LAL.Extended_Return_Stmt'Class)
   is
      SH : LALRW.Node_Rewriting_Handle :=
         LALRW.Handle (Stmt.F_Stmts.F_Stmts);
   begin
      Pop_Objects (SH, True);
   end Handle_Return_Stmt;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Aliased_Absent =>
            Handle_Aliased_Annot (Node.As_Aliased_Absent);
         when LALCO.Ada_Handled_Stmts =>
            Handle_Handled_Stmts (Node.As_Handled_Stmts);
         when LALCO.Ada_Extended_Return_Stmt =>
            Handle_Return_Stmt (Node.As_Extended_Return_Stmt);
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
