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
with Session;

procedure Generate_Types_Interfaces
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   function Starts_With
     (Str, Prefix : Langkit_Support.Text.Text_Type) return Boolean
   is (Str'Length >= Prefix'Length
       and then Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix);

   function Visitor_Name
     (Typ                 : LAL.Base_Type_Decl'Class;
      Is_Ref              : Boolean           := True;
      Referenced_From     : LAL.Analysis_Unit := Unit)
      return Langkit_Support.Text.Text_Type renames Utils.Visitor_Name;

   function Visitor_Overriding_Qualifier
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type
   is
      Has_Public_Base : Boolean :=
        (for some T of Typ.P_Ancestor_Types
            => not T.P_Is_Private
               and then Session.Is_File_To_Process
                          (LAL.Get_Filename (T.Unit)));
   begin
      return
        (if Has_Public_Base
         then "overriding "
         else "");
   end Visitor_Overriding_Qualifier;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   Decl_Part_Count : Node_Counters.Counter;

   type RWNode_Processor is access procedure (X : LALRW.Node_Rewriting_Handle);

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
      Base : LAL.Base_Type_Decl := Decl.P_Base_Subtype;
   begin
      if Base.Kind in LALCO.Ada_Classwide_Type_Decl then
         Base := Base.As_Classwide_Type_Decl.Parent.As_Base_Type_Decl;
      end if;
      return
         Base.Unit /= Unit
         or else Handled_Types.Contains (Base.As_Ada_Node);
   end Is_Handled;

   procedure Generate_Visitor_Prototype
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
   begin
      Append (LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " (X : System.Address);",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule));
   end Generate_Visitor_Prototype;

   procedure Generate_Private_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
      procedure Generate_Renaming (Classwide : Boolean)
      is
         Suffix : Langkit_Support.Text.Text_Type :=
           (if Classwide then "_Classwide" else "");
      begin
         Append (LALRW.Create_From_Template
           (RH,
            "procedure " & Visit_Name & Suffix & " (X : System.Address) "
            & "renames " & Visitor_Name (Decl.P_Full_View) & Suffix
            & ";",
            (1 .. 0 => <>),
            LALCO.Basic_Decl_Rule));
      end Generate_Renaming;

      Is_Tagged : Boolean := Decl.P_Is_Tagged_Type;
   begin
      Generate_Visitor_Prototype (Visit_Name, Decl, Append);
      Generate_Renaming (False);
      if Is_Tagged then
         Generate_Visitor_Prototype
           (Visit_Name & "_Classwide", Decl, Append);
         Generate_Renaming (True);
      end if;
   end Generate_Private_Type_Visitor;

   procedure Generate_Access_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
      Element_Type : LAL.Base_Type_Decl'Class :=
         Decl.P_Accessed_Type;

      Element_Type_Name : Langkit_Support.Text.Text_Type :=
         Utils.Generate_Type_Reference (Element_Type);

      Is_Generalized : Langkit_Support.Text.Text_Type :=
        (if Utils.Is_Generalized_Access_Type (Decl) then "True" else "False");
   begin
      Generate_Visitor_Prototype (Visit_Name, Decl, Append);

      Append (LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & "_Implem is new AGC.Visit_Access_Type ("
        & Element_Type_Name & ", "
        & Is_Generalized & ", "
        & Visitor_Name (Element_Type) & ");",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule));

      Append (LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " (X : System.Address) "
        & "renames " & Visit_Name & "_Implem;",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule));

      if Decl.Child (4).Is_Null then
         LALRW.Set_Child
           (LALRW.Handle (Decl),
            4,
            LALRW.Create_From_Template
              (RH, "with Storage_Pool => AGC.Storage.Get.Pool",
               (1 .. 0 => <>), LALCO.Aspect_Spec_Rule));
      end if;
   end Generate_Access_Type_Visitor;

   procedure Generate_Record_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
      use type Langkit_Support.Text.Unbounded_Text_Type;

      Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      Rec_Def : LAL.Base_Record_Def'Class :=
         Utils.Get_Record_Def (Decl.As_Type_Decl);

      Is_Tagged : Boolean := Decl.P_Is_Tagged_Type;

      CW_Visit_Name : Langkit_Support.Text.Text_Type :=
         Visit_Name & "_Classwide";

      procedure Handle_Base_Record
        (Stmts : LALRW.Node_Rewriting_Handle)
      is
         Base_Type : LAL.Base_Type_Decl'Class :=
            Decl.P_Base_Type;
      begin
         if
            not Base_Type.Is_Null and then
            not Base_Type.P_Is_Interface_Type
         then
            LALRW.Append_Child (Stmts, LALRW.Create_From_Template
              (RH,
               Visitor_Name (Base_Type) & "(X);",
               (1 .. 0 => <>),
               LALCO.Call_Stmt_Rule));
         end if;
      end Handle_Base_Record;

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

                  Comp_Type_Ref : Langkit_Support.Text.Text_Type :=
                     Utils.Generate_Type_Reference (Comp_Type);

                  Comp_Name : LAL.Defining_Name :=
                     Comp.P_Defining_Name;

                  Comp_Text : Langkit_Support.Text.Text_Type :=
                     LAL.Text (Comp_Name);
               begin
                  if Utils.Is_Relevant_Type (Comp_Type) then
                     LALRW.Append_Child (Stmts, LALRW.Create_From_Template
                       (RH,
                        "declare"
                        & "   C : aliased " & Comp_Type_Ref
                        & " := R." & Comp_Text & ";"
                        & "begin "
                        & Visitor_Name (Comp_Type) & "(C'Address);"
                        & "end;",
                        (1 .. 0 => <>), LALCO.Block_Stmt_Rule));
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

      procedure Generate_Visitor_Body is
         Full_Type : Langkit_Support.Text.Text_Type :=
            Type_Name & (if Is_Tagged then "'Class" else "");
         Res : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
           (RH,
            "procedure " & Visit_Name
            & "(X : System.Address) is "
            & "pragma Suppress (Accessibility_Check);"
            & "type Rec_Access is access all " & Full_Type & ";"
            & "for Rec_Access'Size use Standard'Address_Size;"
            & "function Conv is new Ada.Unchecked_Conversion"
            & "  (System.Address, Rec_Access);"
            & "R : " & Full_Type & " renames Conv (X).all;"
            &" begin null; end;",
            (1 .. 0 => <>),
            LALCO.Basic_Decl_Rule);
      begin
         LALRW.Remove_Child
           (LALRW.Child (LALRW.Child (Res, 5), 1), 1);
         Handle_Base_Record
           (LALRW.Child (LALRW.Child (Res, 5), 1));
         Handle_Component_List
           (LALRW.Child (LALRW.Child (Res, 5), 1),
            Rec_Def.F_Components);
         Append (Res);
      end Generate_Visitor_Body;

      procedure Generate_Dispatcher (For_Body : Boolean) is
         Spec : Langkit_Support.Text.Text_Type :=
            Visitor_Overriding_Qualifier (Decl)
            & "procedure AGC_Visit (X : access " & Type_Name & ")";
      begin
         if For_Body then
            Append (LALRW.Create_From_Template
              (RH,
               Spec & "is begin " & Visit_Name & " (X.all'Address); end;",
               (1 .. 0 => <>),
               LALCO.Basic_Decl_Rule));
         else
            Append (LALRW.Create_From_Template
              (RH, Spec & ";", (1 .. 0 => <>), LALCO.Basic_Decl_Rule));
         end if;
      end Generate_Dispatcher;

      procedure Generate_Classwide_Visitor_Body is
         Res : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
           (RH,
            "procedure " & CW_Visit_Name
            & "(X : System.Address) is "
            & "pragma Suppress (Accessibility_Check);"
            & "type T_Access is access all " & Type_Name & "'Class;"
            & "for T_Access'Size use Standard'Address_Size;"
            & "function Conv is new Ada.Unchecked_Conversion"
            & "  (System.Address, T_Access);"
            &" begin Conv (X).AGC_Visit; end;",
            (1 .. 0 => <>),
            LALCO.Basic_Decl_Rule);
      begin
         Append (Res);
      end Generate_Classwide_Visitor_Body;
   begin
      if not Is_Handled (Decl) then
         Delay_Handling (Decl.As_Ada_Node, Decl.As_Ada_Node);
         Generate_Visitor_Prototype (Visit_Name, Decl, Append);
         if Is_Tagged then
            Generate_Dispatcher (For_Body => False);
            Generate_Visitor_Prototype (CW_Visit_Name, Decl, Append);
         end if;
         return;
      end if;

      Generate_Visitor_Body;
      if Is_Tagged then
         Generate_Dispatcher (For_Body => True);
         Generate_Classwide_Visitor_Body;
      end if;
   end Generate_Record_Type_Visitor;

   procedure Generate_Array_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
      N_Dims : Integer := Decl.P_Array_Ndims;

      function Index_Type_Names
        (Dim : Natural := 0) return Langkit_Support.Text.Text_Type
      is
         Dim_Text : Langkit_Support.Text.Text_Type :=
            LAL.Text (Decl.P_Index_Type (Dim).F_Name);
      begin
         if Dim = N_Dims - 1 then
            return Dim_Text;
         else
            return Dim_Text & ", "
               & Langkit_Support.Text.Text_Type'(Index_Type_Names (Dim + 1));
         end if;
      end Index_Type_Names;

      Is_Constrained : Boolean := Decl.P_Is_Definite_Subtype;

      Element_Type : LAL.Base_Type_Decl'Class :=
         Decl.P_Comp_Type;

      Element_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Element_Type.F_Name);

      Array_Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      AGC_Dim : Langkit_Support.Text.Text_Type :=
         Utils.To_String (N_Dims);

      Generic_Visitor_Name : Langkit_Support.Text.Text_Type :=
        (if Is_Constrained
         then "AGC.Visit_Constrained_Array_" & AGC_Dim & "_Type"
         else "AGC.Visit_Unconstrained_Array_" & AGC_Dim & "_Type");
   begin
      Append (LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " is new " & Generic_Visitor_Name &" ("
        & Element_Type_Name & ", "
        & Index_Type_Names & ", "
        & Array_Type_Name & ", "
        & Visitor_Name (Element_Type) & ");",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule));
   end Generate_Array_Type_Visitor;

   procedure Generate_Interface_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
      use type Langkit_Support.Text.Unbounded_Text_Type;

      Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      CW_Visit_Name : Langkit_Support.Text.Text_Type :=
         Visit_Name & "_Classwide";

      procedure Generate_Dispatcher is
      begin
         Append (LALRW.Create_From_Template
           (RH,
            Visitor_Overriding_Qualifier (Decl)
            & "procedure AGC_Visit (X : access "
            & Type_Name
            & ") is abstract;",
            (1 .. 0 => <>), LALCO.Basic_Decl_Rule));
      end Generate_Dispatcher;

      procedure Generate_Classwide_Visitor_Body is
         Res : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
           (RH,
            "procedure " & CW_Visit_Name
            & "(X : System.Address) is "
            & "pragma Suppress (Accessibility_Check);"
            & "type T_Access is access all " & Type_Name & "'Class;"
            & "for T_Access'Size use Standard'Address_Size;"
            & "function Conv is new Ada.Unchecked_Conversion"
            & "  (System.Address, T_Access);"
            &" begin Conv (X).AGC_Visit; end;",
            (1 .. 0 => <>),
            LALCO.Basic_Decl_Rule);
      begin
         Append (Res);
      end Generate_Classwide_Visitor_Body;
   begin
      if not Is_Handled (Decl) then
         Delay_Handling (Decl.As_Ada_Node, Decl.As_Ada_Node);
         Generate_Dispatcher;
         Generate_Visitor_Prototype (CW_Visit_Name, Decl, Append);
      else
         Generate_Classwide_Visitor_Body;
      end if;
   end Generate_Interface_Type_Visitor;

   procedure Generate_Formal_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
   begin
      Append (LALRW.Create_From_Template
        (RH,
        "with procedure " & Visit_Name & " (X : System.Address);",
        (1 .. 0 => <>),
        LALCO.Generic_Formal_Decl_Rule));
   end Generate_Formal_Type_Visitor;

   procedure Generate_Visitors
     (Decl       : LAL.Base_Type_Decl'Class;
      Append     : RWNode_Processor)
   is
      Visit_Name : Langkit_Support.Text.Text_Type :=
         Visitor_Name (Decl, Is_Ref => False);
   begin
      if Decl.P_Is_Generic_Formal then
         Generate_Formal_Type_Visitor (Visit_Name, Decl, Append);
      elsif Decl.P_Is_Private then
         Generate_Private_Type_Visitor (Visit_Name, Decl, Append);
      elsif Decl.P_Is_Access_Type then
         Generate_Access_Type_Visitor (Visit_Name, Decl, Append);
      elsif Decl.P_Is_Record_Type then
         Generate_Record_Type_Visitor (Visit_Name, Decl, Append);
      elsif Decl.P_Is_Array_Type then
         Generate_Array_Type_Visitor (Visit_Name, Decl, Append);
      elsif Decl.P_Is_Interface_Type then
         Generate_Interface_Type_Visitor (Visit_Name, Decl, Append);
      else
         raise Program_Error with "Unhandled type";
      end if;
   end Generate_Visitors;

   procedure Handle_Type_Decl
     (Decl : LAL.Base_Type_Decl'Class; Base_Index : Integer := -1)
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         Utils.Get_Type_Name (Decl);

      Is_Generic_Formal : Boolean := Decl.P_Is_Generic_Formal;

      Decl_Part : LAL.Ada_Node :=
        (if Is_Generic_Formal
         then Decl.Parent.Parent.As_Ada_Node
         else Decl.Parent.As_Ada_Node);

      DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decl_Part);

      Index : Natural :=
        (if Base_Index = -1
         then Decl.Child_Index
         else Base_Index) + Node_Counters.Get (Decl_Part_Count, Decl_Part);

      procedure Insert_Visitor (Visitor : LALRW.Node_Rewriting_Handle) is
      begin
         LALRW.Insert_Child (DH, Index + 2, Visitor);
         Node_Counters.Increase (Decl_Part_Count, Decl_Part);
         Index := Index + 1;
      end Insert_Visitor;

      procedure Append_Visitor (Visitor : LALRW.Node_Rewriting_Handle) is
      begin
         LALRW.Append_Child (DH, Visitor);
      end Append_Visitor;

      Add_Visitor : RWNode_Processor :=
        (if Is_Generic_Formal
         then Append_Visitor'Unrestricted_Access
         else Insert_Visitor'Unrestricted_Access);
   begin
      if Decl.Kind
         in LALCO.Ada_Incomplete_Type_Decl
          | LALCO.Ada_Subtype_Decl
      then
         return;
      elsif not Utils.Is_Relevant_Type (Decl) then
         null;
      elsif
         Decl.P_Is_Access_Type
         and then not Is_Handled (Decl.P_Accessed_Type)
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
      else
         Generate_Visitors (Decl, Add_Visitor);
      end if;

      if not Handled_Types.Contains (Decl.As_Ada_Node) then
         Handled_Types.Insert (Decl.As_Ada_Node);

         for Delayed of Get_Delayed_Types (Decl.As_Ada_Node) loop
            Handle_Type_Decl
              (Delayed.As_Type_Decl,
               (if Base_Index = -1 then Decl.Child_Index else Base_Index));
         end loop;
      end if;
   end Handle_Type_Decl;

   procedure Handle_Package_Instantiation
     (Inst : LAL.Generic_Package_Instantiation'Class)
   is
      Inst_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Inst.F_Name);

      G_Pkg : LAL.Generic_Package_Decl'Class :=
         Inst.P_Designated_Generic_Decl.As_Generic_Package_Decl;

      function Replace_Dots
        (X : Langkit_Support.Text.Text_Type)
         return Langkit_Support.Text.Text_Type
      is
         Ret : Langkit_Support.Text.Text_Type := X;
      begin
         for C of Ret loop
            if C = '.' then
               C := '_';
            end if;
         end loop;
         return Ret;
      end Replace_Dots;

      function Instantiate_Visitor_Package
        (Pkg_Name : Langkit_Support.Text.Text_Type)
         return LALRW.Node_Rewriting_Handle
      is
         use type Langkit_Support.Text.Unbounded_Text_Type;

         Zipped : LAL.Param_Actual_Array :=
            Inst.F_Params.P_Zip_With_Params;

         Params : Langkit_Support.Text.Unbounded_Text_Type;
      begin
         for Param_Actual of Zipped loop
            declare
               Param_Name  : LAL.Defining_Name'Class :=
                  LAL.Param (Param_Actual);
               Param_Decl  : LAL.Basic_Decl'Class :=
                  Param_Name.P_Basic_Decl;
               Actual : LAL.Expr'Class := LAL.Actual (Param_Actual);
            begin
               if Param_Decl.Kind in LALCO.Ada_Base_Type_Decl then
                  Params :=
                     Params & ", "
                     & "AGC_Visit_" & LAL.Text (Param_Name) & " => "
                     & Visitor_Name
                         (Actual.As_Name.P_Name_Designated_Type);
               end if;
            end;
         end loop;
         return LALRW.Create_From_Template
           (RH,
            "package AGC_" & Inst_Name & "_Visitors "
            & "is new " & Pkg_Name
            & "(" & Inst_Name
            & Langkit_Support.Text.To_Text (Params) & ");",
            (1 .. 0 => <>),
            LALCO.Basic_Decl_Rule);
      end Instantiate_Visitor_Package;

      FQN : Langkit_Support.Text.Text_Type :=
         G_Pkg.P_Fully_Qualified_Name;

      Decl_Part : LAL.Ada_Node := Inst.Parent.As_Ada_Node;

      DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decl_Part);

      Index : Natural :=
         Inst.Child_Index + Node_Counters.Get (Decl_Part_Count, Decl_Part);
   begin
      if Starts_With (FQN, "Ada.Containers") then
         LALRW.Insert_Child
           (DH, Index + 2, Instantiate_Visitor_Package
              ("AGC.Standard." & Replace_Dots (FQN) & "_Visitors"));
         Node_Counters.Increase (Decl_Part_Count, Decl_Part);
      end if;
   end Handle_Package_Instantiation;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Anonymous_Type =>
            return LALCO.Over;
         when LALCO.Ada_Generic_Formal_Package =>
            return LALCO.Over;
         when LALCO.Ada_Base_Type_Decl =>
            Handle_Type_Decl (Node.As_Base_Type_Decl);
         when LALCO.Ada_Generic_Package_Instantiation =>
            Handle_Package_Instantiation
              (Node.As_Generic_Package_Instantiation);
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
