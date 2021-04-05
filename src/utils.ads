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

   function Is_Relevant_Root (Decl : LAL.Object_Decl'Class) return Boolean;

   function Is_Managed (Typ : LAL.Base_Type_Decl'Class) return Boolean;

   function Is_Alias (Decl : LAL.Object_Decl'Class) return Boolean;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class;

   function Enclosing_Subp_Body
     (N : LAL.Ada_Node'Class) return LAL.Base_Subp_Body;

   function Is_Actual_Expr (Expr : LAL.Expr'Class) return Boolean;

   function Is_Named_Expr (Expr : LAL.Expr'Class) return Boolean;

   function Get_Body
     (Decl : LAL.Basic_Decl'Class) return LAL.Body_Node;

   function Is_Generalized_Access_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean;

   function Generate_Type_Reference
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type;

   function Unique_Identifier
     (Decl : LAL.Basic_Decl'Class) return Langkit_Support.Text.Text_Type;

   function Get_Type_Name
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type;

   function Visitor_Name
     (Typ                 : LAL.Base_Type_Decl'Class;
      Is_Ref              : Boolean           := True;
      Referenced_From     : LAL.Analysis_Unit := LAL.No_Analysis_Unit)
      return Langkit_Support.Text.Text_Type;

   function Register_Name
     (Typ                 : LAL.Base_Type_Decl'Class;
      Is_Ref              : Boolean           := True)
      return Langkit_Support.Text.Text_Type;

   function Child_Index
     (Node : LALRW.Node_Rewriting_Handle) return Natural;

   procedure Force_Set_Child
     (Parent : LALRW.Node_Rewriting_Handle;
      Child  : Positive;
      Node   : LALRW.Node_Rewriting_Handle);

   function To_String (X : Integer) return Langkit_Support.Text.Text_Type;

   Storage_Pool_Symbol : Langkit_Support.Text.Unbounded_Text_Type :=
      Langkit_Support.Text.To_Unbounded_Text ("Storage_Pool");
end Utils;
