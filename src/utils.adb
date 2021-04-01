with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

with Analysis;
with Post_Actions;
with Session;

package body Utils is
   function Is_Access_To_Value_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean
   is (Typ.P_Is_Access_Type
       and then Typ.P_Access_Def.Kind not in LALCO.Ada_Access_To_Subp_Def);

   function Is_Relevant_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean
   is
      Full_Typ : LAL.Base_Type_Decl;

      function Is_Known_Irrelevant return Boolean is
         use type Langkit_Support.Text.Unbounded_Text_Type;

         Unit : LAL.Compilation_Unit := Full_Typ.P_Enclosing_Compilation_Unit;
         FQN  : LAL.Unbounded_Text_Type_Array :=
            Unit.P_Syntactic_Fully_Qualified_Name;
      begin
         if FQN'Length >= 2 then
            if FQN (1) = "ada" then
               if FQN (2) = "exceptions" then
                  return True;
               elsif FQN (2) = "strings" then
                  return True;
               end if;
            end if;
         end if;
         return False;
      end Is_Known_Irrelevant;
   begin
      if Typ.Is_Null then
         return False;
      end if;

      Full_Typ := Typ.P_Base_Subtype.P_Full_View;

      if Is_Access_To_Value_Type (Full_Typ) then
         return True;
      elsif Is_Known_Irrelevant then
         return False;
      else
         return Full_Typ.P_Is_Record_Type
             or else (Full_Typ.P_Is_Array_Type
                      and then Is_Relevant_Type (Full_Typ.P_Comp_Type))
             or else (Full_Typ.P_Is_Classwide
                      and then Is_Relevant_Type (Full_Typ.Parent.As_Base_Type_Decl))
             or else Full_Typ.P_Is_Interface_Type
             or else Full_Typ.P_Is_Generic_Formal;
      end if;
   end Is_Relevant_Type;

   function Is_Relevant_Root (Decl : LAL.Object_Decl'Class) return Boolean is
      Type_Expr : LAL.Type_Expr := Decl.P_Type_Expression;
      Type_Decl : LAL.Base_Type_Decl := Type_Expr.P_Designated_Type_Decl;
   begin
      return Is_Relevant_Type (Type_Decl) and then not Is_Alias (Decl);
   end Is_Relevant_Root;

   function Is_Alias (Decl : LAL.Object_Decl'Class) return Boolean is
      Is_Constant : Boolean :=
         Decl.F_Has_Constant.Kind in LALCO.Ada_Constant_Present;
   begin
      return not Decl.F_Renaming_Clause.Is_Null
             or else (Is_Constant
                      and then Utils.Is_Named_Expr (Decl.F_Default_Expr));
   end is_Alias;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class
   is
      use type LAL.Ada_Node;
      use all type LAL.Expr;

      Parent       : LAL.Ada_Node := N.Parent;
      Grand_Parent : LAL.Ada_Node :=
        (if Parent.Is_Null then LAL.No_Ada_Node else Parent.Parent);
   begin
      if Grand_Parent.Is_Null then
         return LAL.No_Ada_Node;
      elsif Grand_Parent.Kind in LALCO.Ada_Declarative_Part_Range then
         return N;
      elsif Parent.Kind in LALCO.Ada_Stmt_List then
         return N;
      elsif Parent.Kind in LALCO.Ada_Case_Expr_Alternative then
         return N;
      elsif Parent.Kind in LALCO.Ada_Elsif_Expr_Part then
         return N;
      elsif Parent.Kind in LALCO.Ada_If_Expr
            and then N.Kind not in LALCO.Ada_Elsif_Expr_Part_List
            and then N /= Parent.As_If_Expr.F_Cond_Expr
      then
         return N;
      elsif Parent.Kind in LALCO.Ada_Bin_Op
            and then Parent.As_Bin_Op.F_Op.Kind
               in LALCO.Ada_Op_And_Then | LALCO.Ada_Op_Or_Else
            and then N = Parent.As_Bin_Op.F_Right
      then
         return N;
      else
         return Find_Scope (Parent);
      end if;
   end Find_Scope;

   function Enclosing_Subp_Body
     (N : LAL.Ada_Node'Class) return LAL.Base_Subp_Body
   is
   begin
      if N.Kind in LALCO.Ada_Base_Subp_Body then
         return N.As_Base_Subp_Body;
      elsif not N.Parent.Is_Null then
         return Enclosing_Subp_Body (N.Parent);
      else
         return LAL.No_Base_Subp_Body;
      end if;
   end Enclosing_Subp_Body;

   function Is_Actual_Expr (Expr : LAL.Expr'Class) return Boolean is
      use LAL;
   begin
      if Expr.Kind in LALCO.Ada_Box_Expr then
         return False;
      elsif Expr.Kind in LALCO.Ada_Name
         and then Expr.As_Name.P_Is_Defining
      then
         return False;
      elsif
         Expr.Kind in LALCO.Ada_Name
         and then Expr.Parent.Kind in LALCO.Ada_Call_Expr
         and then Expr.Parent.As_Call_Expr.F_Name = Expr
      then
         declare
            N : LAL.Name := Expr.As_Name;
            P : LAL.Name := N.Parent.As_Name;
         begin
            if
               N.P_Is_Call
               and then N.P_Called_Subp_Spec = P.P_Called_Subp_Spec
            then
               return False;
            elsif not N.P_Name_Designated_Type.Is_Null then
               return False;
            end if;
         end;
      elsif
         Expr.Kind in LALCO.Ada_Name
         and then Expr.Parent.Kind in LALCO.Ada_Dotted_Name
         and then Expr.Parent.As_Dotted_Name.F_Suffix = Expr
      then
         return False;
      elsif
         Expr.Kind in LALCO.Ada_Qual_Expr
         or else Expr.Parent.Kind in LALCO.Ada_Qual_Expr
      then
         return False;
      end if;
      return True;
   end Is_Actual_Expr;

   function Is_Named_Expr (Expr : LAL.Expr'Class) return Boolean is
      use type Analysis.Summaries_Access;
   begin
      case Expr.Kind is
         when LALCO.Ada_Identifier =>
            declare
               Decl : LAL.Basic_Decl'Class :=
                  Expr.As_Identifier.P_Referenced_Decl;
            begin
               return
                  Decl.Kind in
                     LALCO.Ada_Object_Decl_Range
                   | LALCO.Ada_For_Loop_Var_Decl
                   | LALCO.Ada_Param_Spec
                   | LALCO.Ada_Component_Decl
                   | LALCO.Ada_Enum_Literal_Decl;
            end;

         when LALCO.Ada_Dotted_Name =>
            return Is_Named_Expr (Expr.As_Dotted_Name.F_Suffix);

         when LALCO.Ada_Paren_Expr =>
            return Is_Named_Expr (Expr.As_Paren_Expr.F_Expr);

         when LALCO.Ada_Null_Literal | LALCO.Ada_Num_Literal =>
            return True;

         when LALCO.Ada_Call_Expr =>
            if not Expr.As_Call_Expr.P_Is_Call then
               return True;
            end if;

            if Analysis.Summaries = null then
               return False;
            end if;

            return not Analysis.Does_Allocate
              (Get_Body
                 (Expr.As_Call_Expr.P_Called_Subp_Spec.Parent.As_Basic_Decl));

         when LALCO.Ada_Explicit_Deref =>
            return True;

         when LALCO.Ada_Attribute_Ref =>
            return Expr.As_Attribute_Ref.P_Is_Access_Attribute;

         when others =>
            return False;
      end case;
   end Is_Named_Expr;

   function Get_Body
     (Decl : LAL.Basic_Decl'Class) return LAL.Body_Node
   is
   begin
      return (if Decl.Kind in LALCO.Ada_Body_Node
              then Decl.As_Body_Node
              else Decl.P_Body_Part_For_Decl);
   end Get_Body;

   function Is_Generalized_Access_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean
   is
   begin
      if Typ.Kind in LALCO.Ada_Type_Decl then
         declare
            Def : LAL.Type_Def'Class := Typ.As_Type_Decl.F_Type_Def;
         begin
            return
               Def.Kind in LALCO.Ada_Type_Access_Def
               and then Def.As_Type_Access_Def.F_Has_All.Kind
                           in LALCO.Ada_All_Present;
         end;
      end if;
      return False;
   end Is_Generalized_Access_Type;

   function Generate_Type_Reference
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type
   is
      Is_Classwide : Boolean := Typ.Kind in LALCO.Ada_Classwide_Type_Decl;

      Base : Langkit_Support.Text.Text_Type :=
        (if Is_Classwide
         then Typ.Parent.As_Base_Type_Decl.P_Fully_Qualified_Name
         else Typ.P_Fully_Qualified_Name);

      Suffix : Langkit_Support.Text.Text_Type :=
        (if Is_Classwide
         then "'Class"
         else "");
   begin
      return Base & Suffix;
   end Generate_Type_Reference;

   function Unique_Identifier
     (Decl : LAL.Basic_Decl'Class) return Langkit_Support.Text.Text_Type
   is
      Name : Langkit_Support.Text.Text_Type := Decl.P_Fully_Qualified_Name;
   begin
      for C of Name loop
         if C = '.' then
            C := '_';
         end if;
      end loop;
      return Name;
   end Unique_Identifier;

   function Is_Runtime_Unit (U : LAL.Compilation_Unit) return Boolean is
      use type Langkit_Support.Text.Unbounded_Text_Type;

      FQN : LAL.Unbounded_Text_Type_Array :=
         U.P_Syntactic_Fully_Qualified_Name;
   begin
      return FQN (FQN'First) = Langkit_Support.Text.To_Unbounded_Text ("ada");
   end Is_Runtime_Unit;

   function Fully_Qualified_Decl_Part_Of
     (Decl : LAL.Basic_Decl'Class) return Langkit_Support.Text.Text_Type
   is
      FQN : Langkit_Support.Text.Text_Type :=
         (if Decl.Kind in LALCO.Ada_Classwide_Type_Decl
          then Decl.Parent.As_Base_Type_Decl.P_Fully_Qualified_Name
          else Decl.P_Fully_Qualified_Name);
   begin
      for I in reverse FQN'First .. FQN'Last loop
         if FQN (I) = '.' then
            return FQN (FQN'First .. I);
         end if;
      end loop;
      return FQN;
   end Fully_Qualified_Decl_Part_Of;

   function Get_Type_Name
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type
   is
   begin
      if Typ.Kind in LALCO.Ada_Anonymous_Type_Decl_Range then
         if Typ.P_Is_Access_Type then
            return "AGC_Anon_Access_To_" & Get_Type_Name (Typ.P_Accessed_Type);
         elsif Typ.P_Is_Array_Type then
            raise Program_Error with "Anonymous array types not handled.";
         else
            raise Program_Error with "Unhandled type.";
         end if;
      else
         return LAL.Text (Typ.P_Defining_Name);
      end if;
   end Get_Type_Name;

   function Is_Subtype_Decl (X : LAL.Base_Type_Decl'Class) return Boolean is
     (X.Kind in LALCO.Ada_Subtype_Decl or else
      (X.Kind in LALCO.Ada_Classwide_Type_Decl and then
       X.As_Classwide_Type_Decl.Parent.Kind in LALCO.Ada_Subtype_Decl));

   function Visitor_Name
     (Typ                 : LAL.Base_Type_Decl'Class;
      Is_Ref              : Boolean           := True;
      Referenced_From     : LAL.Analysis_Unit := LAL.No_Analysis_Unit)
      return Langkit_Support.Text.Text_Type
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         Get_Type_Name (Typ);

      Is_Standard_Type : Boolean :=
         Is_Runtime_Unit (Typ.P_Enclosing_Compilation_Unit);

      function Relevant_Qualified_Decl_Part_Of
        (Decl  : LAL.Basic_Decl'Class;
         First : Boolean := True) return Langkit_Support.Text.Text_Type
      is
         use type LAL.Analysis_Unit;
      begin
         if Is_Runtime_Unit (Decl.P_Enclosing_Compilation_Unit) then
            declare
               Parent : LAL.Basic_Decl := Decl.P_Parent_Basic_Decl;
            begin
               if Parent.Unit = Decl.P_Standard_Unit then
                  return "AGC.Standard.";
               elsif Is_Runtime_Unit (Parent.P_Enclosing_Compilation_Unit) then
                  return Relevant_Qualified_Decl_Part_Of (Parent, False)
                     & LAL.Text (Parent.P_Defining_Name.P_Relative_Name)
                     & (if First then "_Visitors." else "_");
               else
                  return Relevant_Qualified_Decl_Part_Of (Parent, False)
                     & "AGC_"
                     & LAL.Text (Parent.P_Defining_Name.P_Relative_Name)
                     & "_Visitors.";
               end if;
            end;
         else
            return Fully_Qualified_Decl_Part_Of (Decl);
         end if;
      end Relevant_Qualified_Decl_Part_Of;

      function Normalized_Name
        (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type
      is (if    Typ.P_Is_Classwide
          then  Normalized_Name (Typ.Parent.As_Base_Type_Decl) & "_Classwide"
          elsif Typ.P_Is_Private
          then  Type_Name & "_Private"
          else  Type_Name);

      procedure Handle_Reference (Result : Langkit_Support.Text.Text_Type) is
         use type LAL.Analysis_Unit;
      begin
         if not Session.Is_File_To_Process (LAL.Get_Filename (Typ.Unit))
            and then Referenced_From /= LAL.No_Analysis_Unit
            and then not Is_Standard_Type
         then
            Session.To_Do.Register
              (Post_Actions.Generate_External_Interface_Action'
                 (Typ.Unit,
                  Langkit_Support.Slocs.Start_Sloc
                    (LAL.Sloc_Range (Typ))));

            Session.To_Do.Register
              (Post_Actions.Add_With_Clause_Action'
                 (Referenced_From,
                  Langkit_Support.Text.To_Unbounded_Text (Result)));
         end if;
      end Handle_Reference;
   begin
      if Is_Relevant_Type (Typ) then
         if Is_Subtype_Decl (Typ) then
            return Visitor_Name (Typ.P_Base_Subtype, Is_Ref, Referenced_From);
         elsif Is_Ref then
            return Result : Langkit_Support.Text.Text_Type :=
               Relevant_Qualified_Decl_Part_Of (Typ)
               & "AGC_Visit_" & Normalized_Name (Typ)
            do
               Handle_Reference (Result);
            end return;
         else
            return "AGC_Visit_" & Normalized_Name (Typ);
         end if;
      else
         return "AGC.No_Op";
      end if;
   end Visitor_Name;

   function Child_Index
     (Node : LALRW.Node_Rewriting_Handle) return Natural
   is
      use type LALRW.Node_Rewriting_Handle;

      Parent : LALRW.Node_Rewriting_Handle := LALRW.Parent (Node);
	begin
      for I in 1 .. LALRW.Children_Count (Parent) loop
         if LALRW.Child (Parent, I) = Node then
            return I;
         end if;
      end loop;
		return 0;
   end Child_Index;

   function To_String (X : Integer) return Langkit_Support.Text.Text_Type is
      Img : Langkit_Support.Text.Text_Type := X'Wide_Wide_Image;
   begin
      return Img (Img'First + 1 .. Img'Last);
   end To_String;
end Utils;
