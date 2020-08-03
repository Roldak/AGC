with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

with Node_Counters;
with Utils;

procedure Generate_Types_Interfaces
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   Decl_Part_Count : Node_Counters.Counter;

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, LAL.Ada_Node, LAL."=");

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   package Node_Multi_Maps is new Ada.Containers.Hashed_Maps
     (LAL.Ada_Node, Node_Vectors.Vector, LAL.Hash, LAL."=", Node_Vectors."=");

   Handled_Types : Node_Sets.Set;
   Delayed_Types : Node_Multi_Maps.Map;

   procedure Delay_Handling (K, E : LAL.Ada_Node)
   is
      use type Node_Multi_Maps.Cursor;

      Cursor : Node_Multi_Maps.Cursor := Delayed_Types.Find (K);
   begin
      if Cursor = Node_Multi_Maps.No_Element then
         Delayed_Types.Insert
           (K, Node_Vectors.To_Vector (E, 1));
      else
         declare
            V : Node_Vectors.Vector := Node_Multi_Maps.Element (Cursor);
         begin
            V.Append (E);
            Delayed_Types.Replace_Element (Cursor, V);
         end;
      end if;
   end Delay_Handling;

   function Get_Delayed_Types (K : LAL.Ada_Node) return Node_Vectors.Vector
   is
      use type Node_Multi_Maps.Cursor;

      Cursor : Node_Multi_Maps.Cursor := Delayed_Types.Find (K);
   begin
      if Cursor = Node_Multi_Maps.No_Element then
         return Node_Vectors.Empty_Vector;
      else
         return Node_Multi_Maps.Element (Cursor);
      end if;
   end Get_Delayed_Types;

   function Is_Handled (Decl : LAL.Base_Type_Decl'Class) return Boolean is
      use type LAL.Analysis_Unit;
   begin
      return
         Decl.Unit /= Unit
         or else Handled_Types.Contains (Decl.As_Ada_Node);
   end Is_Handled;

   function Generate_Visitor_Prototype
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class)
      return LALRW.Node_Rewriting_Handle
   is
   begin
      return LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " (X : System.Address);",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule);
   end Generate_Visitor_Prototype;

   function Generate_Access_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class)
      return LALRW.Node_Rewriting_Handle
   is
      Element_Type : LAL.Base_Type_Decl'Class :=
         Decl.P_Accessed_Type;

      Element_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Element_Type.F_Name);

      Access_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);
   begin
      return LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " is new AGC.Visit_Access_Type ("
        & Element_Type_Name & ", "
        & Access_Type_Name & ", "
        & Utils.Visitor_Name (Element_Type) & ");",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule);
   end Generate_Access_Type_Visitor;

   function Generate_Record_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class)
      return LALRW.Node_Rewriting_Handle
   is
      use type Langkit_Support.Text.Unbounded_Text_Type;

      Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      Rec_Def : LAL.Base_Record_Def'Class :=
         Decl.As_Type_Decl.F_Type_Def.As_Record_Type_Def.F_Record_Def;

      procedure Handle_Component_List
        (Stmts : LALRW.Node_Rewriting_Handle; List : LAL.Component_List'Class)
      is
         Comps : LAL.Ada_Node_List := List.F_Components;
      begin
         for I in 1 .. Comps.Children_Count loop
            if Comps.Child (I).Kind in LALCO.Ada_Base_Formal_Param_Decl then
               declare
                  Comp : LAL.Base_Formal_Param_Decl'Class :=
                     Comps.Child (I).As_Base_Formal_Param_Decl;

                  Comp_Type : LAL.Base_Type_Decl'Class :=
                     Comp.P_Formal_Type;

                  Comp_Type_Ref : LALRW.Node_Rewriting_Handle :=
                     Utils.Generate_Type_Reference (RH, Comp_Type);

                  Comp_Name : LAL.Defining_Name :=
                     Comp.P_Defining_Name;

                  Comp_Text : Langkit_Support.Text.Text_Type :=
                     LAL.Text (Comp_Name);
               begin
                  if Utils.Is_Relevant_Type (Comp_Type) then
                     LALRW.Append_Child (Stmts, LALRW.Create_From_Template
                       (RH,
                        "declare"
                        & "   C : aliased {} := R." & Comp_Text & ";"
                        & "begin "
                        & Utils.Visitor_Name (Comp_Type) & "(C'Address);"
                        & "end;",
                        (1 => Comp_Type_Ref), LALCO.Block_Stmt_Rule));
                  end if;
               end;
            end if;
         end loop;

         if not List.F_Variant_Part.Is_Null then
            declare
               Var_Part : LAL.Variant_Part := List.F_Variant_Part;
               Variants : LAL.Variant_List := Var_Part.F_Variant;

               Discr_Name : Langkit_Support.Text.Text_Type :=
                  LAL.Text (Var_Part.F_Discr_Name);

               Case_Stmt : LALRW.Node_Rewriting_Handle :=
                  LALRW.Create_From_Template
                    (RH,
                     "case R." & Discr_Name & " is "
                     & "   when others => null;"
                     & "end case;",
                     (1 .. 0 => <>),
                     LALCO.Case_Stmt_Rule);

               Alts : LALRW.Node_Rewriting_Handle :=
                  LALRW.Child (Case_Stmt, 2);
            begin
               LALRW.Remove_Child (Alts, 1);
               for J in 1 .. Variants.Children_Count loop
                  declare
                     Variant : LAL.Variant := Variants.Child (J).As_Variant;

                     Alt_Stmts : LALRW.Node_Rewriting_Handle :=
                        LALRW.Create_Regular_Node
                          (RH, LALCO.Ada_Stmt_List, (1 .. 0 => <>));

                     Alt : LALRW.Node_Rewriting_Handle :=
                        LALRW.Create_Case_Stmt_Alternative
                          (RH,
                           LALRW.Clone (LALRW.Handle (Variant.F_Choices)),
                           Alt_Stmts);
                  begin
                     Handle_Component_List (Alt_Stmts, Variant.F_Components);
                     LALRW.Append_Child (Alts, Alt);
                  end;
               end loop;

               LALRW.Append_Child (Stmts, Case_Stmt);
            end;
         end if;

         if LALRW.Children_Count (Stmts) = 0 then
            LALRW.Append_Child (Stmts, LALRW.Create_Regular_Node
              (RH, LALCO.Ada_Null_Stmt, (1 .. 0 => <>)));
         end if;
      end Handle_Component_List;
   begin
      if not Is_Handled (Decl) then
         Delay_Handling (Decl.As_Ada_Node, Decl.As_Ada_Node);
         return Generate_Visitor_Prototype (Visit_Name, Decl);
      end if;

      return Res : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
        (RH,
         "procedure " & Visit_Name
         & "(X : System.Address) is "
         & "pragma Suppress (Accessibility_Check);"
         & "type Rec_Access is access all " & Type_Name & ";"
         & "for Rec_Access'Size use Standard'Address_Size;"
         & "function Conv is new Ada.Unchecked_Conversion"
         & "  (System.Address, Rec_Access);"
         & "R : aliased " & Type_Name & " := Conv (X).all;"
         &" begin null; end;",
         (1 .. 0 => <>),
         LALCO.Basic_Decl_Rule)
      do
         LALRW.Remove_Child
           (LALRW.Child (LALRW.Child (Res, 5), 1), 1);
         Handle_Component_List
           (LALRW.Child (LALRW.Child (Res, 5), 1),
            Rec_Def.F_Components);
      end return;
   end Generate_Record_Type_Visitor;

   function Generate_Array_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class)
      return LALRW.Node_Rewriting_Handle
   is
      Is_Constrained : Boolean := Decl.P_Is_Definite_Subtype;

      Element_Type : LAL.Base_Type_Decl'Class :=
         Decl.P_Comp_Type;

      Index_Type : LAL.Base_Type_Decl'Class :=
         Decl.P_Index_Type (0);

      Element_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Element_Type.F_Name);

      Index_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Index_Type.F_Name);

      Array_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      Generic_Visitor_Name : Langkit_Support.Text.Text_Type :=
        (if Is_Constrained
         then "AGC.Visit_Constrained_Array_Type"
         else "AGC.Visit_Unconstrained_Array_Type");
   begin
      return LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " is new " & Generic_Visitor_Name &" ("
        & Element_Type_Name & ", "
        & Index_Type_Name & ", "
        & Array_Type_Name & ", "
        & Utils.Visitor_Name (Element_Type) & ");",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule);
   end Generate_Array_Type_Visitor;

   function Generate_Visitor
     (Decl       : LAL.Base_Type_Decl'Class) return LALRW.Node_Rewriting_Handle
   is
      Visit_Name : Langkit_Support.Text.Text_Type :=
         Utils.Visitor_Name (Decl, Is_Ref => False);
   begin
      if Decl.P_Is_Access_Type then
         return Generate_Access_Type_Visitor (Visit_Name, Decl);
      elsif Decl.P_Is_Record_Type then
         return Generate_Record_Type_Visitor (Visit_Name, Decl);
      elsif Decl.P_Is_Array_Type then
         return Generate_Array_Type_Visitor (Visit_Name, Decl);
      else
         raise Program_Error with "Unhandled type";
      end if;
   end Generate_Visitor;

   procedure Handle_Type_Decl
     (Decl : LAL.Base_Type_Decl'Class; Base_Index : Integer := -1)
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      Decl_Part : LAL.Ada_Node := Decl.Parent.As_Ada_Node;

      DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decl_Part);

      Index : Natural :=
        (if Base_Index = -1
         then Decl.Child_Index
         else Base_Index) + Node_Counters.Get (Decl_Part_Count, Decl_Part);
   begin
      if Decl.Kind in LALCO.Ada_Incomplete_Type_Decl then
         return;
      elsif not Utils.Is_Relevant_Type (Decl) then
         Handled_Types.Insert (Decl.As_Ada_Node);
         return;
      elsif
         Decl.P_Is_Access_Type and then not Is_Handled (Decl.P_Accessed_Type)
      then
         Delay_Handling
           (Decl.P_Accessed_Type.As_Ada_Node,
            Decl.As_Ada_Node);
         return;
      elsif
         Decl.P_Is_Array_Type and then not Is_Handled (Decl.P_Comp_Type)
      then
         Delay_Handling
           (Decl.P_Comp_Type.As_Ada_Node,
            Decl.As_Ada_Node);
         return;
      end if;

      LALRW.Insert_Child
        (DH, Index + 2, Generate_Visitor (Decl));
      Node_Counters.Increase (Decl_Part_Count, Decl_Part);

      if not Handled_Types.Contains (Decl.As_Ada_Node) then
         Handled_Types.Insert (Decl.As_Ada_Node);

         for Delayed of Get_Delayed_Types (Decl.As_Ada_Node) loop
            Handle_Type_Decl
              (Delayed.As_Type_Decl,
               (if Base_Index = -1 then Decl.Child_Index else Base_Index));
         end loop;
      end if;
   end Handle_Type_Decl;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Base_Type_Decl =>
            Handle_Type_Decl (Node.As_Base_Type_Decl);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
   if not LALRW.Apply (RH).Success then
      raise Program_Error
         with "generate_types_interfaces: could not apply rewritings";
   end if;
end Generate_Types_Interfaces;
