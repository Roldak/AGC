with Ada.Text_IO; use Ada.Text_IO;

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

   function Generate_No_Op_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class)
      return LALRW.Node_Rewriting_Handle
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);
   begin
      return LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " is new GC.No_Op ("
        & Type_Name & ");",
        (1 .. 0 => <>),
        LALCO.Basic_Decl_Rule);
   end Generate_No_Op_Visitor;

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
        "procedure " & Visit_Name & " is new GC.Visit_Access_Type ("
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

      Comps : LAL.Ada_Node_List'Class :=
         Rec_Def.F_Components.F_Components;

      Holes : Langkit_Support.Text.Unbounded_Text_Type;
      Fills : LALRW.Node_Rewriting_Handle_Array (1 .. Comps.Children_Count);
   begin
      if Comps.Children_Count = 0 then
         return Generate_No_Op_Visitor (Visit_Name, Decl);
      end if;

      for I in 1 .. Comps.Children_Count loop
         Holes := Holes & "{}";
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
            Fills (I) := LALRW.Create_From_Template
              (RH,
               "declare"
               & "   C : aliased {} := R." & Comp_Text & ";"
               & "begin "
               & Utils.Visitor_Name (Comp_Type) & "(C'Address);"
               & "end;",
               (1 => Comp_Type_Ref), LALCO.Block_Stmt_Rule);
         end;
      end loop;

      return LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name
        & "(X : System.Address) is "
        & "pragma Suppress (Accessibility_Check);"
        & "type Rec_Access is access all " & Type_Name & ";"
        & "for Rec_Access'Size use Standard'Address_Size;"
        & "function Conv is new Ada.Unchecked_Conversion"
        & "  (System.Address, Rec_Access);"
        & "R : aliased " & Type_Name & " := Conv (X).all;"
        &" begin "
        & Langkit_Support.Text.To_Text (Holes)
        & " end;",
        Fills,
        LALCO.Basic_Decl_Rule);
   end Generate_Record_Type_Visitor;

   function Generate_Array_Type_Visitor
     (Visit_Name : Langkit_Support.Text.Text_Type;
      Decl       : LAL.Base_Type_Decl'Class)
      return LALRW.Node_Rewriting_Handle
   is
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
   begin
      return LALRW.Create_From_Template
        (RH,
        "procedure " & Visit_Name & " is new GC.Visit_Array_Type ("
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
         Utils.Visitor_Name (Decl);
   begin
      if Decl.P_Is_Access_Type then
         return Generate_Access_Type_Visitor (Visit_Name, Decl);
      elsif Decl.P_Is_Record_Type then
         return Generate_Record_Type_Visitor (Visit_Name, Decl);
      elsif Decl.P_Is_Array_Type then
         return Generate_Array_Type_Visitor (Visit_Name, Decl);
      else
         return Generate_No_Op_Visitor (Visit_Name, Decl);
      end if;
   end Generate_Visitor;

   procedure Handle_Type_Decl
     (Decl : LAL.Base_Type_Decl'Class)
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Decl.F_Name);

      Decl_Part : LAL.Ada_Node := Decl.Parent.As_Ada_Node;

      DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decl_Part);

      Index : Natural :=
         Decl.Child_Index + Node_Counters.Get
           (Decl_Part_Count, Decl_Part);
   begin
      LALRW.Insert_Child
        (DH, Index + 2, LALRW.Create_From_Template
          (RH,
           "function AGC_Register is new GC.Register ("
           & Type_Name
           & ");",
           (1 .. 0 => <>),
           LALCO.Basic_Decl_Rule));
      Node_Counters.Increase (Decl_Part_Count, Decl_Part);

      LALRW.Insert_Child
        (DH, Index + 3, Generate_Visitor (Decl));
      Node_Counters.Increase (Decl_Part_Count, Decl_Part);
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
