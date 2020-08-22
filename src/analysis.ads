with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Hashed_Maps;

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

   procedure Get
     (Self : in out Summary_Holder;
      Does_Allocate : out Boolean) is abstract;

   type Summaries_Map is synchronized new Summaries_Holder with private;

   Summaries : Summaries_Access;

private
   protected type Local_Summary is new Summary_Holder with
      procedure Set_Target (T : Libadalang.Analysis.Basic_Decl);
      overriding procedure Get (Does_Allocate : out Boolean);
   private
      Target      : Libadalang.Analysis.Basic_Decl;
      Is_Computed : Boolean := False;
      Allocates   : Boolean;
   end Local_Summary;

   subtype Key is Unbounded_Text_Type;

   function Hash (X : Key) return Ada.Containers.Hash_Type;

   package Local_Summaries is new Ada.Containers.Hashed_Maps
     (Key, Summary_Access, Hash, Ada.Strings.Wide_Wide_Unbounded."=", "=");

   protected type Summaries_Map is new Summaries_Holder with
      overriding procedure Get_Summary
        (Subprogram : Libadalang.Analysis.Basic_Decl'Class;
         Summary    : out Summary_Access);
   private
      Map : Local_Summaries.Map;
   end Summaries_Map;
end Analysis;
