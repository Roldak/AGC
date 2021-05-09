with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Rewriting;

package Pass is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   function Rewriting_Handle
     (Unit : LAL.Analysis_Unit) return LALRW.Rewriting_Handle;

   procedure Apply_Rewritings
     (Unit       : LAL.Analysis_Unit;
      Error_Name : String);
end Pass;
