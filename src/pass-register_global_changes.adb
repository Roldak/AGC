with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Session;
with Post_Actions;
with Utils;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Unparsing;

procedure Pass.Register_Global_Changes
  (Job_Ctx : Libadalang.Helpers.App_Job_Context;
   Unit    : Libadalang.Analysis.Analysis_Unit)
is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALU    renames Libadalang.Unparsing;

   function Is_AGC_Decl (Node : LAL.Basic_Decl'Class) return Boolean is
     (Utils.Starts_With (LAL.Text (Node.P_Defining_Name), "AGC_"));

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
         if not Is_AGC_Decl (Node) then
            return;
         end if;
      end if;

      if not GGP.Is_Null and then GGP.Kind in LALCO.Ada_Base_Package_Decl then
         Post_Actions.Actions.Move
           ((Unit => Node.Unit,
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
               Post_Actions.Actions.Add_Generic_Actual
                 ((Unit => Args.Unit,
                   Sloc => Start_Sloc (LAL.Sloc_Range (Args)),
                   Fix  => To_Unbounded_Text
                     (Utils.Visitor_Name (Param.As_Base_Type_Decl, False)
                      & " => "
                      & Utils.Visitor_Name
                        (Actual.As_Name.P_Name_Designated_Type,
                         Referenced_From => Unit))));
            elsif Param.Kind in LALCO.Ada_Generic_Package_Instantiation then
               declare
                  F_Inst : LAL.Generic_Package_Instantiation :=
                     Param.As_Generic_Package_Instantiation;

                  F_Name : Langkit_Support.Text.Text_Type :=
                     LAL.Text (F_Inst.F_Name);

                  F_Decl : LAL.Generic_Package_Decl'Class :=
                     F_Inst.P_Designated_Generic_Decl.As_Generic_Package_Decl;

                  FQN : Langkit_Support.Text.Text_Type :=
                     F_Decl.P_Fully_Qualified_Name;

                  A_Inst : LAL.Generic_Instantiation :=
                     Actual.As_Name.P_Referenced_Decl.As_Generic_Instantiation;

                  A_Decl : LAL.Generic_Package_Decl :=
                     A_Inst.P_Designated_Generic_Decl.As_Generic_Package_Decl;

                  A_Name : LAL.Defining_Name := A_Decl.P_Defining_Name;
               begin
                  if Utils.Starts_With (FQN, "Ada.Containers") then
                     Post_Actions.Actions.Add_Generic_Actual
                       ((Unit => Args.Unit,
                         Sloc => Start_Sloc (LAL.Sloc_Range (Args)),
                         Fix  => To_Unbounded_Text
                           ("AGC_" & F_Name & "_Visitors => "
                            & Utils.Visitor_Package (A_Name))));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Fix_Generic_Instantiation;

   procedure Handle_Container_Package_Instantiation
     (Inst : LAL.Generic_Package_Instantiation'Class;
      FQN  : Langkit_Support.Text.Text_Type)
   is
      use Langkit_Support.Slocs;
      use Langkit_Support.Text;

      function Actuals return Text_Type is
         use type Unbounded_Text_Type;

         Result : Unbounded_Text_Type;
      begin
         for Param_Actual of Inst.F_Params.P_Zip_With_Params loop
            declare
               Param_Name  : LAL.Defining_Name'Class :=
                  LAL.Param (Param_Actual);
               Param_Decl  : LAL.Basic_Decl'Class :=
                  Param_Name.P_Basic_Decl;
               Actual : LAL.Expr'Class := LAL.Actual (Param_Actual);
            begin
               if Param_Decl.Kind in LALCO.Ada_Base_Type_Decl then
                  Result :=
                     Result & ", "
                     & "AGC_Visit_" & LAL.Text (Param_Name) & " => "
                     & Utils.Visitor_Name
                         (Actual.As_Name.P_Name_Designated_Type,
                          Referenced_From => Inst.Unit);
               end if;
            end;
         end loop;
         return To_Text (Result);
      end Actuals;

      Inst_Name        : Text_Type :=
         LAL.Text (Inst.P_Defining_Name);

      Visitor_Pkg_Name : Text_Type :=
         "AGC.Standard." & Replace_Dots (FQN) & "_Visitors";
   begin
      Post_Actions.Actions.Add_Basic_Decl_After
        ((Unit => Inst.Unit,
          Sloc => Start_Sloc (LAL.Sloc_Range (Inst)),
          Fix  => To_Unbounded_Text
            ("package AGC_" & Inst_Name & "_Visitors is new "
             & Visitor_Pkg_Name & "(" & Inst_Name & Actuals & ");")));
   end Handle_Container_Package_Instantiation;

   procedure Handle_Generic_Instantiation
     (Node : LAL.Generic_Instantiation'Class)
   is
      Name : Langkit_Support.Text.Text_Type := LAL.Text (Node.P_Defining_Name);
   begin
      if Name'Length >= 10
         and then Name (Name'First .. Name'First + 2) = "AGC"
      then
         if Name (Name'Last - 5.. Name'Last) = "Implem" then
            Move_Body (Node);
         end if;
      else
         declare
            Gen_Decl : LAL.Generic_Decl :=
               Node.P_Designated_Generic_Decl.As_Generic_Decl;

            FQN : Langkit_Support.Text.Text_Type :=
               Gen_Decl.P_Fully_Qualified_Name;
         begin
            if Utils.Starts_With (FQN, "Ada.Containers") then
               Handle_Container_Package_Instantiation
                 (Node.As_Generic_Package_Instantiation, FQN);
            else
               Fix_Generic_Instantiation (Node);
            end if;
         end;
      end if;
   end Handle_Generic_Instantiation;

   procedure Handle_Generic_Formal_Type_Decl
     (Typ : LAL.Base_Type_Decl'Class)
   is
      use Langkit_Support.Slocs;
      use Langkit_Support.Text;

      Visitor_Name : Text_Type := Utils.Visitor_Name (Typ, Is_Ref => False);
   begin
      Post_Actions.Actions.Add_Generic_Formal
        ((Unit => Typ.Unit,
          Sloc => Start_Sloc (LAL.Sloc_Range (Typ)),
          Fix  => To_Unbounded_Text
            ("with procedure " & Visitor_Name & " (X : System.Address);")));
   end Handle_Generic_Formal_Type_Decl;

   procedure Handle_Formal_Container_Package
     (Inst : LAL.Generic_Package_Instantiation'Class;
      FQN  : Langkit_Support.Text.Text_Type)
   is
      use Langkit_Support.Slocs;
      use Langkit_Support.Text;

      Inst_Name        : Text_Type :=
         LAL.Text (Inst.P_Defining_Name);

      Visitor_Pkg_Name : Text_Type :=
         "AGC.Standard." & Replace_Dots (FQN) & "_Visitors";
   begin
      Post_Actions.Actions.Add_Generic_Formal
        ((Unit => Inst.Unit,
          Sloc => Start_Sloc (LAL.Sloc_Range (Inst)),
          Fix  => To_Unbounded_Text
            ("with package AGC_" & Inst_Name & "_Visitors is new "
             & Visitor_Pkg_Name & "(" & Inst_Name & ", others => <>);")));
   end Handle_Formal_Container_Package;

   procedure Handle_Generic_Formal_Package
     (Inst : LAL.Generic_Package_Instantiation'Class)
   is
      use Langkit_Support.Slocs;
      use Langkit_Support.Text;

      P : LAL.Assoc_List := Inst.F_Params;

      Gen_Decl : LAL.Basic_Decl := Inst.P_Designated_Generic_Decl;

      FQN : Langkit_Support.Text.Text_Type := Gen_Decl.P_Fully_Qualified_Name;
   begin
      if Utils.Starts_With (FQN, "Ada.Containers") then
         Handle_Formal_Container_Package (Inst, FQN);
      elsif
         not Session.Is_File_To_Process (LAL.Get_Filename (Gen_Decl.Unit))
      then
         return;
      end if;

      for I in 1 .. P.Children_Count loop
         if P.Child (I).As_Param_Assoc.F_R_Expr.Kind in LALCO.Ada_Box_Expr then
            return;
         end if;
      end loop;

      for PM of P.P_Zip_With_Params loop
         if LAL.Param (PM).P_Basic_Decl.Kind in LALCO.Ada_Base_Type_Decl then
            Post_Actions.Actions.Add_Generic_Actual
              ((Unit => Inst.Unit,
                Sloc => Start_Sloc (LAL.Sloc_Range (P)),
                Fix  => To_Unbounded_Text ("<>")));
            return;
         end if;
      end loop;
   end Handle_Generic_Formal_Package;

   procedure Handle_Generic_Formal_Part
     (Node : LAL.Generic_Formal_Part'Class)
   is
   begin
      for Decl of Node.F_Decls loop
         case Decl.Kind is
            when LALCO.Ada_Generic_Formal_Type_Decl =>
               Handle_Generic_Formal_Type_Decl
                 (Decl.As_Generic_Formal.F_Decl.As_Base_Type_Decl);
            when LALCO.Ada_Generic_Formal_Package =>
               Handle_Generic_Formal_Package
                 (Decl.As_Generic_Formal.F_Decl
                  .As_Generic_Package_Instantiation);
            when others =>
               null;
         end case;
      end loop;
   end Handle_Generic_Formal_Part;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Body_Node =>
            Move_Body (Node.As_Basic_Decl);
         when LALCO.Ada_Generic_Formal_Part =>
            Handle_Generic_Formal_Part (Node.As_Generic_Formal_Part);
            return LALCO.Over;
         when LALCO.Ada_Generic_Instantiation =>
            Handle_Generic_Instantiation (Node.As_Generic_Instantiation);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   Unit.Root.Traverse (Process_Node'Access);
end Pass.Register_Global_Changes;
