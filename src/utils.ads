with Libadalang.Analysis;
with Libadalang.Common;

package Utils is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;

   function Is_Relevant_Root (Decl : LAL.Basic_Decl'Class) return Boolean;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class;
end Utils;
