with Libadalang.Analysis;
with Libadalang.Common;

package body Utils is
   function Is_Relevant_Root (Decl : LAL.Basic_Decl'Class) return Boolean is
      Type_Expr : LAL.Type_Expr := Decl.P_Type_Expression;
      Type_Decl : LAL.Base_Type_Decl := Type_Expr.P_Designated_Type_Decl;
   begin
      return Type_Decl.P_Is_Access_Type;
   end Is_Relevant_Root;
end Utils;
