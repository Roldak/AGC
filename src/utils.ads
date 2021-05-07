with Ada.Containers;

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

   function Defined_In_Subp
     (Subp : LAL.Body_Node'Class;
      Def  : LAL.Defining_Name) return Boolean;

   function Imported_Units
     (Unit        : LAL.Analysis_Unit;
      All_Visible : Boolean := False) return LAL.Analysis_Unit_Array;

   function Is_Actual_Expr (Expr : LAL.Expr'Class) return Boolean;

   function Is_Named_Expr (Expr : LAL.Expr'Class) return Boolean;

   function Expands_To_Loop (Expr : LAL.Expr'Class) return Boolean;

   function Get_Body
     (Decl : LAL.Basic_Decl'Class) return LAL.Body_Node;

   function Is_Generalized_Access_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean;

   function Generate_Type_Reference
     (Typ             : LAL.Base_Type_Decl'Class;
      Referenced_From : LAL.Analysis_Unit := LAL.No_Analysis_Unit)
      return Langkit_Support.Text.Text_Type;

   function Get_Type_Name
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type;

   function Visitor_Package
     (Node  : LAL.Ada_Node'Class;
      First : Boolean := True) return Langkit_Support.Text.Text_Type;

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

   function Starts_With
     (Str, Prefix : Langkit_Support.Text.Text_Type) return Boolean;

   function Dot_Concat
     (X : LAL.Unbounded_Text_Type_Array) return Langkit_Support.Text.Text_Type;

   function Base_Name (Full_Path : String) return String;

   function Cached_Node_Hash
     (N : LAL.Ada_Node) return Ada.Containers.Hash_Type;

   type Diagnostic_Kind is (Error, Warning, Note);

   procedure Output_Diagnostic
     (Node : LAL.Ada_Node'Class;
      Message : Langkit_Support.Text.Text_Type;
      Kind : Diagnostic_Kind);

   Storage_Pool_Symbol : Langkit_Support.Text.Unbounded_Text_Type :=
      Langkit_Support.Text.To_Unbounded_Text ("Storage_Pool");
end Utils;
