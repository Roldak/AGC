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

   procedure Handle_Type_Decl
     (Decl : LAL.Type_Decl'Class)
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
        (DH, Index + 3, LALRW.Create_From_Template
          (RH,
           "function AGC_Temp is new GC.Temp ("
           & Type_Name
           & ");",
           (1 .. 0 => <>),
           LALCO.Basic_Decl_Rule));
      Node_Counters.Increase (Decl_Part_Count, Decl_Part);
   end Handle_Type_Decl;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Type_Decl =>
            Handle_Type_Decl (Node.As_Type_Decl);
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
