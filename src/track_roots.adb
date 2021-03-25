with Ada.Text_IO; use Ada.Text_IO;
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
with Session;

procedure Track_Roots
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   Handled_Parts : Node_Sets.Set;
   Require_Root_Count : Node_Sets.Set;
   Subp_Roots : Node_Counters.Counter;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   function Ends_With_Return_Stmt
     (Stmts : LAL.Stmt_List'Class) return Boolean
   is
      Last_Stmt : LAL.Ada_NOde := Stmts.Child (Stmts.Last_Child_Index);
   begin
      case Last_Stmt.Kind is
         when LALCO.Ada_Return_Stmt =>
            return True;
         when LALCO.Ada_Extended_Return_Stmt =>
            return True;
         when LALCO.Ada_Decl_Block =>
            return Ends_With_Return_Stmt
              (Last_Stmt.As_Decl_Block.F_Stmts.F_Stmts);
         when LALCO.Ada_Begin_Block =>
            return Ends_With_Return_Stmt
              (Last_Stmt.As_Begin_Block.F_Stmts.F_Stmts);
         when LALCO.Ada_Case_Stmt =>
            return (for all Alt of Last_Stmt.As_Case_Stmt.F_Alternatives
                      => Ends_With_Return_Stmt (Alt.F_Stmts));
         when LALCO.Ada_If_Stmt =>
            declare
               If_Stmt : LAL.If_Stmt := Last_Stmt.As_If_Stmt;
            begin
               return
                  Ends_With_Return_Stmt (If_Stmt.F_Then_Stmts)
                  and then not If_Stmt.F_Else_Stmts.Is_Null
                  and then If_Stmt.F_Else_Stmts.Children_Count /= 0
                  and then Ends_With_Return_Stmt (If_Stmt.F_Else_Stmts)
                  and then (for all Alt of If_Stmt.F_Alternatives
                              => Ends_With_Return_Stmt (Alt.F_Stmts));
            end;
         when others =>
            return False;
      end case;
   end Ends_With_Return_Stmt;

   procedure Push_Object
     (DH : LALRW.Node_Rewriting_Handle; X : LAL.Object_Decl)
   is
      Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (X.P_Defining_Name);

      Enclosing_Subp : LAL.Ada_Node :=
         Utils.Enclosing_Subp_Body (X).As_Ada_Node;

      Obj_Type : LAL.Base_Type_Decl'Class :=
         X.P_Type_Expression.P_Designated_Type_Decl;

      Dummy_Count : Natural := Node_Counters.Get (Subp_Roots, Enclosing_Subp);
      Dummy_Index : Langkit_Support.Text.Text_Type :=
         Dummy_Count'Wide_Wide_Image;
      Dummy_Name  : Langkit_Support.Text.Text_Type :=
         "AGC_Dummy_"
         & Dummy_Index (Dummy_Index'First + 1 .. Dummy_Index'Last);
   begin
      LALRW.Insert_Child
        (DH, Utils.Child_Index (LALRW.Handle (X)) + 1,
         LALRW.Create_From_Template
           (RH,
            Dummy_Name & " : constant AGC.Empty_Type := "
            & "AGC.Push_Root ({}'Address, {}'Address);",
            (1 => LALRW.Create_Token_Node
                    (RH, LALCO.Ada_Identifier, Name),
             2 => LALRW.Create_Token_Node
                    (RH, LALCO.Ada_Identifier,
                     Utils.Visitor_Name (Obj_Type, Referenced_From => Unit))),
            LALCO.Object_Decl_Rule));

      Node_Counters.Increase (Subp_Roots, Enclosing_Subp);
   end Push_Object;

   function Root_Count_Name
     (Subp_Level : Boolean) return Langkit_Support.Text.Text_Type
   is
     (if Subp_Level then "AGC_Base_Root_Count"
      else "AGC_Root_Count");

   procedure Store_Root_Count
     (Decls : LALRW.Node_Rewriting_Handle;
      Leaving_Subp : Boolean)
   is
   begin
      LALRW.Insert_Child (Decls, 1, LALRW.Create_From_Template
        (RH,
         Root_Count_Name (Leaving_Subp)
         & " : constant Natural := AGC.Root_Count;",
         (1 .. 0 => <>),
         LALCO.Object_Decl_Rule));
   end Store_Root_Count;

   procedure Pop_Objects
     (Stmts        : LALRW.Node_Rewriting_Handle;
      Leaving_Subp : Boolean;
      Index        : Integer := -1)
   is
   begin
      if LALRW.Children_Count (Stmts) = 1
         and then LALRW.Kind (LALRW.Child (Stmts, 1)) in LALCO.Ada_Null_Stmt
      then
         LALRW.Remove_Child (Stmts, 1);
      end if;

      declare
         Stmt : LALRW.Node_Rewriting_Handle :=
            LALRW.Create_From_Template
              (RH,
               "AGC.Pop_Roots (" & Root_Count_Name (Leaving_Subp) & ");",
               (1 .. 0 => <>),
               LALCO.Call_Stmt_Rule);
      begin
         if Index = -1 then
            LALRW.Append_Child (Stmts, Stmt);
         else
            LALRW.Insert_Child (Stmts, Index, Stmt);
         end if;
      end;
   end Pop_Objects;

   procedure Handle_Aliased_Annot (Node : LAL.Aliased_Absent'Class)
   is
      SH  : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);
   begin
      if Node.Parent.Kind in LALCO.Ada_Object_Decl then
         if Node.Parent.As_Object_Decl.F_Renaming_Clause.Is_Null then
            if Utils.Is_Relevant_Root (Node.Parent.As_Object_Decl) then
               LALRW.Replace
                 (SH, LALRW.Create_Node (RH, LALCO.Ada_Aliased_Present));
            end if;
         end if;
      end if;
   end Handle_Aliased_Annot;

   function Parent_Block_Already_Pops
     (Handled_Stmts : LAL.Handled_Stmts) return Boolean
   is
   begin
      --  if we are the only stmt of our parent block and our parent
      --  block is already handled, no need to pop roots.
      return Handled_Parts.Contains (Handled_Stmts.Parent.Parent)
             and then (Handled_Stmts.Parent.Child_Index
                          = Handled_Stmts.Parent.Parent.Children_Count - 1);
   end Parent_Block_Already_Pops;

   procedure Handle_Declarative_Part (Decl_Part : LAL.Declarative_Part) is
      use type LALCO.Ada_Node_Kind_Type;

      Decls      : LAL.Ada_Node_List := Decl_Part.F_Decls;
      Subp_Level : Boolean :=
         Decl_Part.Parent.Kind in LALCO.Ada_Base_Subp_Body;

      DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decls);

      Has_Any_Root : Boolean := False;

      Next_Sibling  : LAL.Ada_Node := LAL.Next_Sibling (Decl_Part);
      Handled_Stmts : LAL.Handled_Stmts :=
        (if not Next_Sibling.Is_Null
            and then Next_Sibling.Kind in LALCO.Ada_Handled_Stmts
         then Next_Sibling.As_Handled_Stmts
         else LAL.No_Handled_Stmts);
   begin
      for N in Decls.First_Child_Index .. Decls.Last_Child_Index loop
         declare
            C : LAL.Ada_Node := LAL.Child (Decls, N);
         begin
            if C.Kind in LALCO.Ada_Object_Decl then
               if Utils.Is_Relevant_Root (C.As_Object_Decl) then
                  Push_Object (DH, C.As_Object_Decl);
                  Has_Any_Root := True;
               end if;
            elsif C.Kind in LALCO.Ada_Base_Package_Decl then
               --  The package itself cannot pop its roots, so the enclosing
               --  declare block should pop them in its place.
               Has_Any_Root := True;
            end if;
         end;
      end loop;

      if not Handled_Stmts.Is_Null then
         Handled_Parts.Insert (Handled_Stmts.F_Stmts.As_Ada_Node);

         if not Subp_Level
            and then Has_Any_Root
            and then not Parent_Block_Already_Pops (Handled_Stmts)
            and then not Ends_With_Return_Stmt (Handled_Stmts.F_Stmts)
         then
            Require_Root_Count.Include (Decl_Part.As_Ada_Node);
            Pop_Objects (LALRW.Handle (Handled_Stmts.F_Stmts), False);
         end if;
      end if;
   end Handle_Declarative_Part;

   procedure Handle_Begin_Block (Block : LAL.Begin_Block) is
   begin
      Handled_Parts.Insert (Block.F_Stmts.F_Stmts.As_Ada_Node);
   end Handle_Begin_Block;

   procedure Handle_Return_Stmt
     (Stmt : LAL.Return_Stmt'Class)
   is
      SH : LALRW.Node_Rewriting_Handle :=
         LALRW.Handle (Stmt);

      PH : LALRW.Node_Rewriting_Handle :=
         LALRW.Handle (Stmt.Parent);

      Subp_Body : LAL.Ada_Node :=
         Utils.Enclosing_Subp_Body (Stmt).As_Ada_Node;
   begin
      if Node_Counters.Get (Subp_Roots, Subp_Body) > 0 then
         Pop_Objects (PH, True, Utils.Child_Index (SH));
      end if;
   end Handle_Return_Stmt;

   procedure Handle_Extended_Return_Stmt
     (Stmt : LAL.Extended_Return_Stmt'Class)
   is
      SH : LALRW.Node_Rewriting_Handle :=
         LALRW.Handle (Stmt.F_Stmts.F_Stmts);

      Subp_Body : LAL.Ada_Node :=
         Utils.Enclosing_Subp_Body (Stmt).As_Ada_Node;
   begin
      if Node_Counters.Get (Subp_Roots, Subp_Body) > 0 then
         Pop_Objects (SH, True);
      end if;
   end Handle_Extended_Return_Stmt;

   procedure Handle_Exception_Handler (Handler : LAL.Exception_Handler) is
      Handled_Stmts : LAL.Handled_Stmts :=
         Handler.Parent.Parent.As_Handled_Stmts;
      Decl_Part     : LAL.Declarative_Part :=
         LAL.Previous_Sibling (Handled_Stmts).As_Declarative_Part;
      Subp_Level    : Boolean :=
         Handled_Stmts.Parent.Kind in LALCO.Ada_Base_Subp_Body;
   begin
      if (Subp_Level or else not Parent_Block_Already_Pops (Handled_Stmts))
         and then not Ends_With_Return_Stmt (Handler.F_Stmts)
      then
         if not Decl_Part.Is_Null then
            Require_Root_Count.Include (Decl_Part.As_Ada_Node);
         end if;
         Pop_Objects (LALRW.Handle (Handler.F_Stmts), Subp_Level);
      end if;
   end Handle_Exception_Handler;

   procedure Handle_Generic_Instantiation
     (Inst : LAL.Generic_Instantiation'Class)
   is
      Args : LAL.Assoc_List :=
        (if Inst.Kind in LALCO.Ada_Generic_Subp_Instantiation
         then Inst.As_Generic_Subp_Instantiation.F_Params
         else Inst.As_Generic_Package_Instantiation.F_Params);

      Gen_Decl : LAL.Basic_Decl := Inst.P_Designated_Generic_Decl;
   begin
      if not Session.Is_File_To_Process (LAL.Get_Filename (Gen_Decl.Unit)) then
         return;
      end if;

      for Param_Actual of Args.P_Zip_With_Params loop
         declare
            Param  : LAL.Basic_Decl :=
               LAL.Param (Param_Actual).P_Basic_Decl;
            Actual : LAL.Expr'Class :=
               LAL.Actual (Param_Actual);
         begin
            if Param.Kind in LALCO.Ada_Base_Type_Decl then
               LALRW.Append_Child
                 (LALRW.Handle (Args),
                  LALRW.Create_From_Template
                    (RH,
                     Utils.Visitor_Name (Param.As_Base_Type_Decl, False)
                     & " => "
                     & Utils.Visitor_Name
                       (Actual.As_Name.P_Name_Designated_Type,
                        Referenced_From => Unit),
                     (1 .. 0 => <>),
                     LALCO.Param_Assoc_Rule));
            end if;
         end;
      end loop;
   end Handle_Generic_Instantiation;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Aliased_Absent =>
            Handle_Aliased_Annot (Node.As_Aliased_Absent);
         when LALCO.Ada_Declarative_Part_Range =>
            Handle_Declarative_Part (Node.As_Declarative_Part);
         when LALCO.Ada_Begin_Block =>
            Handle_Begin_Block (Node.As_Begin_Block);
         when LALCO.Ada_Return_Stmt =>
            Handle_Return_Stmt (Node.As_Return_Stmt);
         when LALCO.Ada_Extended_Return_Stmt =>
            Handle_Extended_Return_Stmt (Node.As_Extended_Return_Stmt);
         when LALCO.Ada_Exception_Handler =>
            Handle_Exception_Handler (Node.As_Exception_Handler);
         when LALCO.Ada_Generic_Instantiation =>
            Handle_Generic_Instantiation (Node.As_Generic_Instantiation);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;

   procedure Process_Subp_Body (Node : LAL.Ada_Node; Roots : Natural) is
   begin
      if not Node.Is_Null and then Roots > 0 then
         Require_Root_Count.Include (Node.As_Subp_Body.F_Decls.As_Ada_Node);

         if not Ends_With_Return_Stmt (Node.As_Subp_Body.F_Stmts.F_Stmts) then
            Pop_Objects
              (LALRW.Handle (Node.As_Subp_Body.F_Stmts.F_Stmts), True);
         end if;
      end if;
   end Process_Subp_Body;

   procedure Process_Store_Request (Cursor : Node_Sets.Cursor) is
      Decl_Part : LAL.Declarative_Part :=
         Node_Sets.Element (Cursor).As_Declarative_Part;

      Subp_Level : Boolean :=
         Decl_Part.Parent.Kind in LALCO.Ada_Base_Subp_Body;
   begin
      Store_Root_Count (LALRW.Handle (Decl_Part.F_Decls), Subp_Level);
   end Process_Store_Request;
begin
   Unit.Root.Traverse (Process_Node'Access);
   Node_Counters.Iterate (Subp_Roots, Process_Subp_Body'Access);
   Node_Sets.Iterate (Require_Root_Count, Process_Store_Request'Access);
   if not LALRW.Apply (RH).Success then
      raise Program_Error with "track_roots: could not apply rewritings";
   end if;
end Track_Roots;
