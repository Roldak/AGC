with Libadalang.Analysis;
with Libadalang.Common;

package body Utils is
   function Is_Relevant_Root (Decl : LAL.Basic_Decl'Class) return Boolean is
      Type_Expr : LAL.Type_Expr := Decl.P_Type_Expression;
      Type_Decl : LAL.Base_Type_Decl := Type_Expr.P_Designated_Type_Decl;
   begin
      return Type_Decl.P_Is_Access_Type;
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
end Utils;
