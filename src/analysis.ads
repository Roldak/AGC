with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;

package Analysis is

   type Summaries_Holder is synchronized interface;
   type Summaries_Access is access Summaries_Holder'Class;

   type Summary_Holder   is synchronized interface;
   type Summary_Access   is access Summary_Holder'Class;

   procedure Get_Summary
     (Self       : in out Summaries_Holder;
      Subprogram : Libadalang.Analysis.Basic_Decl'Class;
      Summary    : out Summary_Access) is abstract;

   procedure Get_Existing_Summary
     (Self      : in out Summaries_Holder;
      Subp_Name : Unbounded_Text_Type;
      Summary   : out Summary_Access) is abstract;

   type Subp_Info is record
      Subp : Libadalang.Analysis.Body_Node;
      Id   : Unbounded_Text_Type;
   end record;

   function Hash (X : Subp_Info) return Ada.Containers.Hash_Type;

   package Subp_Sets is new Ada.Containers.Hashed_Sets
     (Subp_Info, Hash, "=", "=");

   procedure Get
     (Self          : in out Summary_Holder;
      Does_Allocate : out Boolean;
      Called_Subps  : out Subp_Sets.Set) is abstract;

   procedure Get_Blocking
     (Self          : in out Summary_Holder;
      Does_Allocate : out Boolean;
      Called_Subps  : out Subp_Sets.Set) is abstract;

   type Summaries_Map is synchronized new Summaries_Holder with private;

   Summaries : Summaries_Access;

   function Does_Allocate
     (Subprogram : Libadalang.Analysis.Body_Node'Class) return Boolean;

private
   protected type Local_Summary is new Summary_Holder with
      function Has_Target return Boolean;
      procedure Set_Target (X : Libadalang.Analysis.Basic_Decl);

      overriding procedure Get
        (Does_Allocate : out Boolean;
         Called_Subps  : out Subp_Sets.Set);

      overriding entry Get_Blocking
        (Does_Allocate : out Boolean;
         Called_Subps  : out Subp_Sets.Set);
   private
      Target      : Libadalang.Analysis.Basic_Decl;
      Is_Computed : Boolean := False;
      Allocates   : Boolean;
      Calls       : Subp_Sets.Set;
   end Local_Summary;

   subtype Key is Unbounded_Text_Type;

   function Hash (X : Key) return Ada.Containers.Hash_Type;

   package Local_Summaries is new Ada.Containers.Hashed_Maps
     (Key, Summary_Access, Hash, Ada.Strings.Wide_Wide_Unbounded."=", "=");

   protected type Summaries_Map is new Summaries_Holder with
      overriding procedure Get_Summary
        (Subprogram : Libadalang.Analysis.Basic_Decl'Class;
         Summary    : out Summary_Access);

      overriding procedure Get_Existing_Summary
        (Subp_Name : Unbounded_Text_Type;
         Summary   : out Summary_Access);
   private
      Map : Local_Summaries.Map;
   end Summaries_Map;
end Analysis;
