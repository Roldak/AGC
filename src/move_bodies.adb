with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;

procedure Move_Bodies
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

   procedure Handle_Body (Node : LAL.Body_Node'Class) is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      GGP : LAL.Ada_Node'Class := Node.Parent.Parent.Parent;
   begin
      if not GGP.Is_Null and then GGP.Kind in LALCO.Ada_Base_Package_Decl then
         declare
            Pkg      : LAL.Base_Package_Decl := GGP.As_Base_Package_Decl;
            Pkg_Body : LAL.Package_Body'Class := Pkg.P_Body_Part;

            BH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);
         begin
            if not Pkg_Body.Is_Null then
               LALRW.Remove_Child
                 (LALRW.Handle (Node.Parent), Node.Child_Index + 1);
               LALRW.Insert_Child
                 (LALRW.Handle (Pkg_Body.F_Decls.F_Decls), 1, BH);
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
   if not LALRW.Apply (RH).Success then
      raise Program_Error with "move_bodies: could not apply rewritings";
   end if;
end Move_Bodies;
