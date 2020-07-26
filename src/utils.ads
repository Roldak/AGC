with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Rewriting;

package Utils is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   function Is_Relevant_Type
     (Typ  : LAL.Base_Type_Decl'Class) return Boolean;

   function Is_Relevant_Root (Decl : LAL.Basic_Decl'Class) return Boolean;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class;

   function Enclosing_Subp_Body
     (N : LAL.Ada_Node'Class) return LAL.Base_Subp_Body;

   function Is_Actual_Expr (Expr : LAL.Expr'Class) return Boolean;

   function Is_Named_Expr (Expr : LAL.Expr'Class) return Boolean;

   function Generate_Type_Reference
     (RH  : LALRW.Rewriting_Handle;
      Typ : LAL.Base_Type_Decl'Class) return LALRW.Node_Rewriting_Handle;
end Utils;
