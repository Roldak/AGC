with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Session;
with Post_Actions;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;

procedure Detect_Misplaced_Bodies
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit    : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;

   procedure Handle_Body (Node : LAL.Basic_Decl'Class) is
      use Langkit_Support.Slocs;

      GGP : LAL.Ada_Node'Class := Node.Parent.Parent.Parent;
   begin
      -- do not move null body procedures as they may define an interface
      -- method.
      if Node.Kind in LALCO.Ada_Null_Subp_Decl then
         return;
      end if;

      if not GGP.Is_Null and then GGP.Kind in LALCO.Ada_Base_Package_Decl then
         Session.To_Do.Register (Post_Actions.Move_Action'
           (Unit => Node.Unit,
            Sloc => Start_Sloc (LAL.Sloc_Range (Node))));
      end if;
   end Handle_Body;

   procedure Handle_Instantiation (Node : LAL.Generic_Instantiation'Class) is
      Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Node.P_Defining_Name);
   begin
      if Name'Length >= 10
         and then Name (Name'First .. Name'First + 2) = "AGC"
         and then Name (Name'Last - 5.. Name'Last) = "Implem"
      then
         Handle_Body (Node);
      end if;
   end Handle_Instantiation;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Body_Node =>
            Handle_Body (Node.As_Basic_Decl);
         when LALCO.Ada_Generic_Instantiation =>
            Handle_Instantiation (Node.As_Generic_Instantiation);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
end Detect_Misplaced_Bodies;
