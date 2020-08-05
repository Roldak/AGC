with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Post_Actions;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;

procedure Detect_Misplaced_Bodies
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit    : Libadalang.Analysis.Analysis_Unit;
   To_Do   : in out Post_Actions.Actions)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;

   procedure Handle_Body (Node : LAL.Body_Node'Class) is
      GGP : LAL.Ada_Node'Class := Node.Parent.Parent.Parent;
   begin
      if not GGP.Is_Null and then GGP.Kind in LALCO.Ada_Base_Package_Decl then
         declare
            Pkg      : LAL.Base_Package_Decl  := GGP.As_Base_Package_Decl;
            Pkg_Body : LAL.Package_Body'Class := Pkg.P_Body_Part;
         begin
            if not Pkg_Body.Is_Null then
               To_Do.Register (Post_Actions.Move_Action'
                 (Source => Node.As_Ada_Node,
                  Dest => Pkg_Body.F_Decls.F_Decls.As_Ada_Node));
            end if;
         end;
      end if;
   end Handle_Body;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Body_Node =>
            Handle_Body (Node.As_Body_Node);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
end Detect_Misplaced_Bodies;
