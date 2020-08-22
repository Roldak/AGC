with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Hashes;

with Libadalang.Common;

with Utils;

package body Analysis is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   use type LAL.Analysis_Context;

   function Hash (X : Subp_Info) return Ada.Containers.Hash_Type is
   begin
      return LAL.Hash (X.Subp.As_Ada_Node);
   end Hash;

   function Hash (X : Key) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash (X);
   end Hash;

   protected body Local_Summary is
      function Has_Target return Boolean is (not Target.Is_Null);

      procedure Set_Target (X : Libadalang.Analysis.Basic_Decl) is
      begin
         Target := X;
      end Set_Target;

      procedure Compute_Summary is
         function Process_Node
           (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
         is
         begin
            case Node.Kind is
               when LALCO.Ada_Allocator =>
                  Allocates := True;
               when LALCO.Ada_Name =>
                  declare
                     Called_Spec : LAL.Base_Formal_Param_Holder'Class :=
                        Node.As_Name.P_Called_Subp_Spec;
                  begin
                     if not Called_Spec.Is_Null then
                        declare
                           Subp_Body : LAL.Body_Node := Utils.Get_Body
                             (Called_Spec.Parent.As_Basic_Decl);
                        begin
                           Calls.Include
                             ((Subp_Body,
                               To_Unbounded_Text
                                 (Subp_Body.P_Unique_Identifying_Name)));
                        end;
                     end if;
                  end;
               when others =>
                  null;
            end case;
            return LALCO.Into;
         exception
            when LALCO.Property_Error =>
               Put_Line ("Abandonning analysis of " & LAL.Image (Target));
               return LALCO.Stop;
         end Process_Node;
      begin
         Allocates := False;
         Target.Traverse (Process_Node'Access);
      end Compute_Summary;

      procedure Get
        (Does_Allocate : out Boolean;
         Called_Subps  : out Subp_Sets.Set)
      is
      begin
         if not Is_Computed then
            Put_Line ("Computing summary for " & LAL.Image (Target));
            Compute_Summary;
            Is_Computed := True;
         else
            Put_Line ("Reusing summary");
         end if;

         Does_Allocate := Allocates;
         Called_Subps  := Calls;
      end Get;

      entry Get_Blocking
        (Does_Allocate : out Boolean;
         Called_Subps  : out Subp_Sets.Set)
         when Is_Computed
      is
      begin
         Put_Line ("Reusing summary (potentially waited)");
         Does_Allocate := Allocates;
         Called_Subps  := Calls;
      end Get_Blocking;
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
         else
            Put_Line ("Summary for " & LAL.Image (Subprogram) & " already requested");
         end if;
         Summary := Local_Summaries.Element (Cursor);
         if not Local_Summary (Summary.all).Has_Target then
            Local_Summary (Summary.all).Set_Target (Subprogram.As_Basic_Decl);
         end if;
      end Get_Summary;

      procedure Get_Existing_Summary
        (Subp_Name : Unbounded_Text_Type;
         Summary   : out Summary_Access)
      is
         use type Local_Summaries.Cursor;

         Inserted : Boolean;
         Cursor   : Local_Summaries.Cursor := Map.Find (Subp_Name);
      begin
         if Cursor = Local_Summaries.No_Element then
            Map.Insert (Subp_Name, new Local_Summary, Cursor, Inserted);
         end if;
         Summary := Local_Summaries.Element (Cursor);
      end Get_Existing_Summary;
   end Summaries_Map;

   function Does_Allocate
     (Subprogram : Libadalang.Analysis.Body_Node'Class) return Boolean
   is
      Visited : Subp_Sets.Set;
      Ctx : LAL.Analysis_Context := Subprogram.Unit.Context;

      function Recurse (Info : Subp_Info) return Boolean is
         Summary        : Summary_Access;
         Self_Allocates : Boolean;
         Called_Subps   : Subp_Sets.Set;
      begin
         if Info.Subp.Unit.Context /= Ctx then
            Summaries.Get_Existing_Summary (Info.Id, Summary);
            Summary.Get_Blocking (Self_Allocates, Called_Subps);
         else
            Summaries.Get_Summary (Info.Subp, Summary);
            Summary.Get (Self_Allocates, Called_Subps);
         end if;

         if Self_Allocates then
            return True;
         end if;

         Visited.Insert (Info);

         for Called_Info of Called_Subps loop
            if not Visited.Contains (Called_Info) then
               if Recurse (Called_Info) then
                  return True;
               end if;
            end if;
         end loop;

         return False;
      end Recurse;
   begin
      return Recurse
        ((Subprogram.As_Body_Node,
          To_Unbounded_Text
            (Subprogram.P_Unique_Identifying_Name)));
   end Does_Allocate;
end Analysis;
