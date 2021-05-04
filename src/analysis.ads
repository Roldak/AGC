with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Task_Attributes;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

with GNATCOLL.Traces; use GNATCOLL.Traces;

with Utils;

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
        (Subp : LAL.Body_Node) return Context_Solution;

      with function Default
        (Subp : LAL.Body_Node) return Context_Solution;

      type Universal_Solution is private;

      with function Convert (S : Context_Solution) return Universal_Solution;

      with function "=" (A, B : Context_Solution) return Boolean is <>;
      with function "=" (A, B : Universal_Solution) return Boolean is <>;
   package Shared_Analysis is

      function Get_Context_Solution
        (Subp : LAL.Body_Node) return Context_Solution;

      function Get_Universal_Solution
        (Subp : LAL.Body_Node) return Universal_Solution;

   private

      --  Context solution holder

      package Context_Cache_Maps is new Ada.Containers.Hashed_Maps
        (LAL.Ada_Node, Context_Solution, Utils.Cached_Node_Hash, LAL."=", "=");

      package Context_Solutions_Holder is new Ada.Task_Attributes
        (Context_Cache_Maps.Map, Context_Cache_Maps.Empty_Map);

      --  Universal solution holder

      subtype Universal_Key_Type is Unbounded_Text_Type;
      use Ada.Strings.Wide_Wide_Unbounded;

      package Universal_Cache_Maps is new Ada.Containers.Hashed_Maps
        (Universal_Key_Type, Universal_Solution, Wide_Wide_Hash, "=");

      protected Universal_Solutions_Holder is
         function Contains (Subp : Universal_Key_Type) return Boolean;
         procedure Get
           (Subp     : Universal_Key_Type;
            Solution : out Universal_Solution);
         procedure Insert
           (Subp     : Universal_Key_Type;
            Solution : Universal_Solution);
         procedure Include
           (Subp     : Universal_Key_Type;
            Solution : Universal_Solution);
      private
         Cache : Universal_Cache_Maps.Map;
      end Universal_Solutions_Holder;
   end Shared_Analysis;
end Analysis;
