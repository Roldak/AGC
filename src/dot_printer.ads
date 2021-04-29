with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Multiway_Trees;
with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;

package Dot_Printer is
   type Printer is tagged private;

   subtype Node_Id is Ada.Containers.Hash_Type;

   type Cluster_Path_Type is array (Positive range <>) of Unbounded_Text_Type;

   Empty_Path : constant Cluster_Path_Type;

   procedure Add_Node
     (Self    : in out Printer;
      Id      : Node_Id;
      Name    : Unbounded_Text_Type;
      Cluster : Cluster_Path_Type;
      Color   : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String);

   procedure Add_Edge
     (Self : in out Printer;
      From : Node_Id;
      To   : Node_Id);

   procedure Save
     (Self : Printer;
      Path : String);

private
   package Cluster_Trees is new Ada.Containers.Multiway_Trees
     (Unbounded_Text_Type, "=");

   type Printer is tagged record
      Clusters : Cluster_Trees.Tree;
      Content  : Unbounded_Text_Type;
   end record;

   Empty_Path : constant Cluster_Path_Type := (1 .. 0 => <>);
end Dot_Printer;
