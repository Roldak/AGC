with Langkit_Support.Text;

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

   function Is_Generalized_Access_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean;

   function Get_Record_Def
     (Decl : LAL.Type_Decl'Class) return LAL.Base_Record_Def'Class;

   function Generate_Type_Reference
     (RH  : LALRW.Rewriting_Handle;
      Typ : LAL.Base_Type_Decl'Class) return LALRW.Node_Rewriting_Handle;

   function Unique_Identifier
     (Decl : LAL.Basic_Decl'Class) return Langkit_Support.Text.Text_Type;

   function Visitor_Name
     (Typ : LAL.Base_Type_Decl'Class;
      Is_Ref : Boolean := True) return Langkit_Support.Text.Text_Type;

   function Child_Index
     (Node : LALRW.Node_Rewriting_Handle) return Natural;
end Utils;
