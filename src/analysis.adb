with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Hashes;

package body Analysis is
   package LAL renames Libadalang.Analysis;

   function Hash (X : Key) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash (X);
   end Hash;

   protected body Local_Summary is
      procedure Set_Target (T : Libadalang.Analysis.Basic_Decl) is
      begin
         Target := T;
      end Set_Target;

      procedure Get (Does_Allocate : out Boolean) is
      begin
         if not Is_Computed then
            Put_Line ("Computing summary for " & LAL.Image (Target));
            Allocates := False;
            Is_Computed := True;
         else
            Put_Line ("Reusing summary");
         end if;

         Does_Allocate := Allocates;
      end Get;
   end Local_Summary;

   protected body Summaries_Map is
      procedure Get_Summary
        (Subprogram : LAL.Basic_Decl'Class;
         Summary    : out Summary_Access)
      is
         use type Local_Summaries.Cursor;

         K : Key := To_Unbounded_Text (Subprogram.P_Unique_Identifying_Name);

         Inserted : Boolean;
         Cursor   : Local_Summaries.Cursor := Map.Find (K);
      begin
         if Cursor = Local_Summaries.No_Element then
            Map.Insert (K, new Local_Summary, Cursor, Inserted);
            Local_Summary (Local_Summaries.Element (Cursor).all).Set_Target
              (Subprogram.As_Basic_Decl);
         else
            Put_Line ("Summary for " & LAL.Image (Subprogram) & "already requested");
         end if;
         Summary := Local_Summaries.Element (Cursor);
      end Get_Summary;
   end Summaries_Map;
end Analysis;
