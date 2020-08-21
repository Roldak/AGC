with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Rewriting;

with Node_Counters;
with Utils;

package body Post_Actions is
   package LAL   renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package LALRW renames Libadalang.Rewriting;

   function Lookup
     (Ctx    : LAL.Analysis_Context;
      Action : Move_Action) return LAL.Basic_Decl
   is
      use Langkit_Support.Slocs;

      Unit : LAL.Analysis_Unit :=
         Ctx.Get_From_File (LAL.Get_Filename (Action.Unit));

      Node : LAL.Ada_Node := LAL.Lookup (Unit.Root, Action.Sloc);
   begin
      while Node.Kind not in LALCO.Ada_Basic_Decl loop
         Node := Node.Parent;
      end loop;
      return Node.As_Basic_Decl;
   end Lookup;

   function Find_Destination
     (Node : LAL.Basic_Decl'Class) return LAL.Ada_Node
   is
      GGP      : LAL.Ada_Node'Class     := Node.Parent.Parent.Parent;
      Pkg      : LAL.Base_Package_Decl  := GGP.As_Base_Package_Decl;
      Pkg_Body : LAL.Package_Body'Class := Pkg.P_Body_Part;
   begin
      if not Pkg_Body.Is_Null then
         return Pkg_Body.F_Decls.F_Decls.As_Ada_Node;
      end if;

      return LAL.No_Ada_Node;
   end;

   Insertions : Node_Counters.Counter;

   protected body Actions is
      procedure Register (Action : Move_Action) is
      begin
         To_Move.Append (Action);
      end Register;

      procedure Perform_Actions (Ctx : LAL.Analysis_Context) is
         RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Ctx);
      begin
         for Action of To_Move loop
            declare
               Source : LAL.Basic_Decl := Lookup (Ctx, Action);
               Dest   : LAL.Ada_Node   := Find_Destination (Source);
               Index  : Natural        := Node_Counters.Get (Insertions, Dest);
            begin
               if not Dest.Is_Null then
                  LALRW.Remove_Child
                    (LALRW.Handle (Source.Parent),
                     Utils.Child_Index (LALRW.Handle (Source)));
                  LALRW.Insert_Child
                    (LALRW.Handle (Dest), Index + 1, LALRW.Handle (Source));

                  Node_Counters.Increase (Insertions, Dest);
               end if;
            end;
         end loop;

         if not LALRW.Apply (RH).Success then
            raise Program_Error
               with "post_actions: could not apply rewritings";
         end if;
      end Perform_Actions;
   end Actions;
end Post_Actions;
