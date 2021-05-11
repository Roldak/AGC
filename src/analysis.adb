with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Hashes;

with Utils;

package body Analysis is
   use type LAL.Analysis_Context;

   package body Shared_Analysis is

      function Do_Analyze (Subp : LAL.Body_Node) return Context_Solution is
      begin
         case Subp.Kind is
            when LALCO.Ada_Subp_Renaming_Decl =>
               declare
                  Renamed_Decl : LAL.Basic_Decl :=
                    Subp.As_Subp_Renaming_Decl
                    .F_Renames.F_Renamed_Object.P_Referenced_Decl;

                  Renamed_Body : LAL.Body_Node :=
                    (if Renamed_Decl.Is_Null
                     then LAL.No_Body_Node
                     else Utils.Get_Body (Renamed_Decl));
               begin
                  if Renamed_Body.Is_Null then
                     return Default (Subp);
                  else
                     return Get_Context_Solution (Renamed_Body);
                  end if;
               end;
            when others =>
               return Analyze (Subp);
         end case;
      end Do_Analyze;

      function Get_Context_Solution
        (Subp : LAL.Body_Node) return Context_Solution
      is
         use type Context_Cache_Maps.Cursor;

         Map_Ref : constant Context_Solutions_Holder.Attribute_Handle :=
            Context_Solutions_Holder.Reference;

         Cursor   : Context_Cache_Maps.Cursor;
         Inserted : Boolean;
      begin
         Map_Ref.Insert (Subp.As_Ada_Node, Cursor, Inserted);
         if Inserted then
            declare
               Placeholder : Context_Solution := Default (Subp);
               Key         : Unbounded_Text_Type := To_Unbounded_Text
                 (Subp.P_Unique_Identifying_Name);
            begin
               Map_Ref.Replace_Element (Cursor, Placeholder);

               Universal_Solutions_Holder.Insert (Key, Convert (Placeholder));

               declare
                  Result : Context_Solution := Do_Analyze (Subp);
               begin
                  Map_Ref.Replace_Element (Cursor, Result);
                  Universal_Solutions_Holder.Include (Key, Convert (Result));
                  return Result;
               end;
            end;
         else
            return Context_Cache_Maps.Element (Cursor);
         end if;
      end Get_Context_Solution;

      function Get_Universal_Solution
        (Subp : LAL.Body_Node) return Universal_Solution
      is
         Key : constant Universal_Key_Type :=
            To_Unbounded_Text (Subp.P_Unique_Identifying_Name);

         Result : Universal_Solution;
      begin
         if Universal_Solutions_Holder.Contains (Key) then
            Universal_Solutions_Holder.Get (Key, Result);
         else
            declare
               Context_Result : Context_Solution := Get_Context_Solution (Subp);
            begin
               Universal_Solutions_Holder.Get (Key, Result);
            end;
         end if;
         return Result;
      end Get_Universal_Solution;

      procedure Process_Any_Solution
        (Subp            : LAL.Body_Node;
         Try_Universal   : access function (Result : Universal_Solution)
                                            return Boolean;
         Process_Context : access procedure (Result : Context_Solution))
      is
         use type Context_Cache_Maps.Cursor;

         Map_Ref : constant Context_Solutions_Holder.Attribute_Handle :=
            Context_Solutions_Holder.Reference;

         Cursor : Context_Cache_Maps.Cursor := Map_Ref.Find (Subp.As_Ada_Node);
      begin
         if Cursor = Context_Cache_Maps.No_Element then
            declare
               Key : constant Universal_Key_Type :=
                  To_Unbounded_Text (Subp.P_Unique_Identifying_Name);
            begin
               if Universal_Solutions_Holder.Contains (Key) then
                  declare
                     Result : Universal_Solution;
                  begin
                     Universal_Solutions_Holder.Get (Key, Result);
                     if Try_Universal (Result) then
                        return;
                     end if;
                  end;
               end if;
               Process_Context (Get_Context_Solution (Subp));
            end;
         else
            Process_Context (Context_Cache_Maps.Element (Cursor));
         end if;
      end Process_Any_Solution;

      function Get_Universal_Solution
        (Subp_Name : Unbounded_Text_Type;
         Result    : out Universal_Solution) return Boolean
      is
      begin
         if Universal_Solutions_Holder.Contains (Subp_Name) then
            Universal_Solutions_Holder.Get (Subp_Name, Result);
            return True;
         end if;
         return False;
      end Get_Universal_Solution;

      procedure Iterate_Universal_Solutions
        (Process : access procedure (Subp   : Unbounded_Text_Type;
                                     Result : Universal_Solution))
      is
      begin
         Universal_Solutions_Holder.Iterate (Process);
      end Iterate_Universal_Solutions;

      protected body Universal_Solutions_Holder is
         function Contains (Subp : Universal_Key_Type) return Boolean is
         begin
            return Cache.Contains (Subp);
         end Contains;

         procedure Get
           (Subp     : Universal_Key_Type;
            Solution : out Universal_Solution)
         is
         begin
            Solution := Cache.Element (Subp);
         end Get;

         procedure Insert
           (Subp     : Universal_Key_Type;
            Solution : Universal_Solution)
         is
            Dummy_Cursor : Universal_Cache_Maps.Cursor;
            Dummy_Bool   : Boolean;
         begin
            Cache.Insert (Subp, Solution, Dummy_Cursor, Dummy_Bool);
         end Insert;

         procedure Include
           (Subp     : Universal_Key_Type;
            Solution : Universal_Solution)
         is
         begin
            Cache.Include (Subp, Solution);
         end Include;

         procedure Iterate
           (Process : access procedure (Subp   : Universal_Key_Type;
                                        Result : Universal_Solution))
         is
            procedure Unwrap (C : Universal_Cache_Maps.Cursor) is
            begin
               Process
                 (Universal_Cache_Maps.Key (C),
                  Universal_Cache_Maps.Element (C));
            end Unwrap;
         begin
            Cache.Iterate (Unwrap'Access);
         end Iterate;
      end Universal_Solutions_Holder;

   end Shared_Analysis;
end Analysis;
