with Libadalang.Analysis;
with Libadalang.Common;

package body Utils is
   function Is_Relevant_Type
     (Typ  : LAL.Base_Type_Decl'Class) return Boolean
   is
   begin
      return
         Typ.P_Is_Access_Type
         or else Typ.P_Is_Record_Type
         or else (Typ.P_Is_Array_Type
                  and then Is_Relevant_Type (Typ.P_Comp_Type));
   end Is_Relevant_Type;

   function Is_Relevant_Root (Decl : LAL.Basic_Decl'Class) return Boolean is
      Type_Expr : LAL.Type_Expr := Decl.P_Type_Expression;
      Type_Decl : LAL.Base_Type_Decl := Type_Expr.P_Designated_Type_Decl;
   begin
      return Is_Relevant_Type (Type_Decl);
   end Is_Relevant_Root;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class
   is
      use type LALCO.Ada_Node_Kind_Type;
      use type LAL.Ada_Node;

      Parent       : LAL.Ada_Node := N.Parent;
      Grand_Parent : LAL.Ada_Node :=
        (if Parent.Is_Null then LAL.No_Ada_Node else Parent.Parent);
   begin
      if Grand_Parent.Is_Null then
         return LAL.No_Ada_Node;
      elsif Grand_Parent.Kind = LALCO.Ada_Declarative_Part then
         return N;
      elsif Parent.Kind = LALCO.Ada_Stmt_List then
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
      if
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
   begin
      case Expr.Kind is
         when LALCO.Ada_Identifier =>
            declare
               Decl : LAL.Basic_Decl'Class :=
                  Expr.As_Identifier.P_Referenced_Decl;
            begin
               return
                  Decl.Kind in
                     LALCO.Ada_Object_Decl
                   | LALCO.Ada_Param_Spec
                   | LALCO.Ada_Component_Decl;
            end;

         when LALCO.Ada_Dotted_Name =>
            return Is_Named_Expr (Expr.As_Dotted_Name.F_Suffix);

         when LALCO.Ada_Null_Literal =>
            return True;

         when LALCO.Ada_Call_Expr =>
            return Expr.As_Call_Expr.F_Name.P_Expression_Type.P_Is_Array_Type;

         when others =>
            return False;
      end case;
   end Is_Named_Expr;

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

   function Get_Record_Def
     (Decl : LAL.Type_Decl'Class) return LAL.Base_Record_Def'Class
   is
      Type_Def : LAL.Type_Def'Class := Decl.F_Type_Def;
   begin
      if Type_Def.Kind in LALCO.Ada_Record_Type_Def then
         return Type_Def.As_Record_Type_Def.F_Record_Def;
      elsif Type_Def.Kind in LALCO.Ada_Derived_Type_Def then
         if not Type_Def.As_Derived_Type_Def.F_Record_Extension.Is_Null then
            return Type_Def.As_Derived_Type_Def.F_Record_Extension;
         end if;
      end if;
      return LAL.No_Base_Record_Def;
   end Get_Record_Def;

   function Generate_Type_Reference
     (RH  : LALRW.Rewriting_Handle;
      Typ : LAL.Base_Type_Decl'Class) return LALRW.Node_Rewriting_Handle
   is
   begin
      return LALRW.Create_From_Template
        (RH, LAL.P_Fully_Qualified_Name (Typ),
         (1 .. 0 => <>), LALCO.Type_Expr_Rule);
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

   function Fully_Qualified_Decl_Part_Of
     (Decl : LAL.Basic_Decl'Class) return Langkit_Support.Text.Text_Type
   is
      FQN : Langkit_Support.Text.Text_Type :=
         Decl.P_Fully_Qualified_Name;
   begin
      for I in reverse FQN'First .. FQN'Last loop
         if FQN (I) = '.' then
            return FQN (FQN'First .. I);
         end if;
      end loop;
      return FQN;
   end Fully_Qualified_Decl_Part_Of;

   function Visitor_Name
     (Typ    : LAL.Base_Type_Decl'Class;
      Is_Ref : Boolean := True) return Langkit_Support.Text.Text_Type
   is
   begin
      if Is_Relevant_Type (Typ) then
         if Is_Ref then
            return Fully_Qualified_Decl_Part_Of (Typ)
               & "AGC_Visit_" & LAL.Text (Typ.P_Defining_Name);
         else
            return "AGC_Visit_" & LAL.Text (Typ.P_Defining_Name);
         end if;
      else
         return "AGC.No_Op";
      end if;
   end Visitor_Name;
end Utils;
