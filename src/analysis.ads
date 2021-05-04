with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Task_Identification; use Ada.Task_Identification;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

with GNATCOLL.Traces; use GNATCOLL.Traces;

package Analysis is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   Analysis_Trace : constant Trace_Handle := Create ("AGC.ANALYSIS");

   function Is_Owner_At
     (Var   : Libadalang.Analysis.Defining_Name;
      Place : Libadalang.Analysis.Ada_Node'Class) return Boolean;

   generic
      type Context_Solution is private;
      with function Analyze
        (Subp : LAL.Base_Subp_Body) return Context_Solution;
      with function Default
        (Subp : LAL.Base_Subp_Body) return Context_Solution;
      with function "=" (A, B : Context_Solution) return Boolean is <>;

      type Universal_Solution is private;
      with function Convert (S : Context_Solution) return Universal_Solution;
      with function "=" (A, B : Universal_Solution) return Boolean is <>;
   package Shared_Analysis is

      function Get_Or_Compute
        (Subp : LAL.Base_Subp_Body) return Context_Solution;

      function Get_Or_Return
        (Subp_Name   : Unbounded_Text_Type;
         Result      : out Context_Solution) return Boolean;

   private

      subtype Key_Type is Unbounded_Text_Type;
      use Ada.Strings.Wide_Wide_Unbounded;

      protected type Summary_Type is
         procedure Seize (Ctx : LAL.Analysis_Context);

         procedure Set (R : Context_Solution);
         entry Get (R : out Context_Solution);

         function Get_Computer return LAL.Analysis_Context;
      private
         Result   : Context_Solution;
         Computer : LAL.Analysis_Context := LAL.No_Analysis_Context;
      end Summary_Type;

      type Summary_Access is access Summary_Type;

      package Cache_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type, Summary_Access, Wide_Wide_Hash, "=");

      protected Holder is
         function Get_Access_To_Existing_Summary
           (Subp : Key_Type) return Summary_Access;

         procedure Get_Access_To_Summary
           (Subp         : Key_Type;
            Ctx          : LAL.Analysis_Context;
            Summary      : out Summary_Access;
            Must_Compute : out Boolean);
      private
         Cache : Cache_Maps.Map;
      end Holder;
   end Shared_Analysis;
end Analysis;
