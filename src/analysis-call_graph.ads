with Ada.Containers.Hashed_Sets;

with Langkit_Support.Text; use Langkit_Support.Text;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

package Analysis.Call_Graph is
   package Subp_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   type Context_Solution is record
      Has_Unknown_Calls : Boolean;
      Known_Calls       : Subp_Sets.Set;
   end record;

   function Analyze (Subp : LAL.Base_Subp_Body) return Context_Solution;
   function Default (Subp : LAL.Base_Subp_Body) return Context_Solution is
     ((Has_Unknown_Calls => False,
       Known_Calls       => Subp_Sets.Empty_Set));

   package UID_Sets is new Ada.Containers.Hashed_Sets
     (Unbounded_Text_Type, Wide_Wide_Hash, "=", "=");

   type Universal_Solution is record
      Has_Unknown_Calls : Boolean;
      Known_Calls       : UID_Sets.Set;
   end record;

   function To_Universal (X : Context_Solution) return Universal_Solution;

   package Share is new Shared_Analysis
     (Context_Solution   => Context_Solution,
      Analyze            => Analyze,
      Default            => Default,
      Universal_Solution => Universal_Solution,
      Convert            => To_Universal);
end Analysis.Call_Graph;
