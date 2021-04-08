with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Session;
with Post_Actions;
with Utils;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;

procedure Register_Global_Changes
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit    : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;

   procedure Move_Body (Node : LAL.Basic_Decl'Class) is
      use Langkit_Support.Slocs;

      GGP : LAL.Ada_Node'Class := Node.Parent.Parent.Parent;
   begin
      if Node.Kind in LALCO.Ada_Null_Subp_Decl then
         -- do not move null body procedures as they may define an interface
         -- method.
         return;
      elsif Node.Kind in LALCO.Ada_Subp_Renaming_Decl then
         -- do not move user renaming declarations as they may have different
         -- visibility behaviors in the spec.
         declare
            Name : Langkit_Support.Text.Text_Type :=
               LAL.Text (Node.P_Defining_Name);
         begin
            if Name'Length < 3 or else
               Name (Name'First .. Name'First + 2) /= "AGC"
            then
               return;
            end if;
         end;
      end if;

      if not GGP.Is_Null and then GGP.Kind in LALCO.Ada_Base_Package_Decl then
         Session.To_Do.Register (Post_Actions.Move_Action'
           (Unit => Node.Unit,
            Sloc => Start_Sloc (LAL.Sloc_Range (Node))));
      end if;
   end Move_Body;

   procedure Fix_Generic_Instantiation
     (Inst : LAL.Generic_Instantiation'Class)
   is
      use Langkit_Support.Slocs;
      use Langkit_Support.Text;

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
               Session.To_Do.Register (Post_Actions.Add_Generic_Actual'
                 (Unit => Args.Unit,
                  Sloc => Start_Sloc (LAL.Sloc_Range (Args)),
                  Fix  => To_Unbounded_Text
                    (Utils.Visitor_Name (Param.As_Base_Type_Decl, False)
                     & " => "
                     & Utils.Visitor_Name
                       (Actual.As_Name.P_Name_Designated_Type,
                        Referenced_From => Unit))));
            end if;
         end;
      end loop;
   end Fix_Generic_Instantiation;

   procedure Handle_Generic_Instantiation (Node : LAL.Generic_Instantiation'Class) is
      Name : Langkit_Support.Text.Text_Type := LAL.Text (Node.P_Defining_Name);
   begin
      if Name'Length >= 10
         and then Name (Name'First .. Name'First + 2) = "AGC"
      then
         if Name (Name'Last - 5.. Name'Last) = "Implem" then
            Move_Body (Node);
         end if;
      else
         Fix_Generic_Instantiation (Node);
      end if;
   end Handle_Generic_Instantiation;

   procedure Handle_Generic_Formal_Package
     (Node : LAL.Generic_Formal_Package'Class)
   is
      use Langkit_Support.Slocs;
      use Langkit_Support.Text;

      Inst : LAL.Generic_Package_Instantiation :=
         Node.F_Decl.As_Generic_Package_Instantiation;

      P : LAL.Assoc_List := Inst.F_Params;

      Gen_Decl : LAL.Basic_Decl := Inst.P_Designated_Generic_Decl;
   begin
      if not Session.Is_File_To_Process (LAL.Get_Filename (Gen_Decl.Unit)) then
         return;
      end if;

      for I in 1 .. P.Children_Count loop
         if P.Child (I).As_Param_Assoc.F_R_Expr.Kind in LALCO.Ada_Box_Expr then
            return;
         end if;
      end loop;

      for PM of P.P_Zip_With_Params loop
         if LAL.Param (PM).P_Basic_Decl.Kind in LALCO.Ada_Base_Type_Decl then
            Session.To_Do.Register (Post_Actions.Add_Generic_Actual'
              (Unit => Node.Unit,
               Sloc => Start_Sloc (LAL.Sloc_Range (P)),
               Fix  => To_Unbounded_Text ("<>")));
            return;
         end if;
      end loop;
   end Handle_Generic_Formal_Package;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Body_Node =>
            Move_Body (Node.As_Basic_Decl);
         when LALCO.Ada_Generic_Instantiation =>
            Handle_Generic_Instantiation (Node.As_Generic_Instantiation);
         when LALCO.Ada_Generic_Formal_Package =>
            Handle_Generic_Formal_Package (Node.As_Generic_Formal_Package);
            return LALCO.Over;
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
end Register_Global_Changes;
