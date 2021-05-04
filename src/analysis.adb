with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Hashes;

with Libadalang.Common;

with Analysis.Dataflow;
with Analysis.Lattices.Finite_Node_Sets;
with Utils;

package body Analysis is
   use type LAL.Analysis_Context;

   package Node_Sets renames Analysis.Lattices.Finite_Node_Sets.Node_Sets;

   generic
      Target : LAL.Base_Subp_Body;
   package Ownership_Analysis is
      use Analysis.Lattices;

      procedure Remove_Possibly_Aliased
        (State : in out Node_Sets.Set;
         Expr  : LAL.Expr);

      procedure Handle_Assignment
        (State : in out Node_Sets.Set;
         Dest  : LAL.Name;
         Val   : LAL.Expr);

      package Ownership_Problem is new Dataflow.Problem
        (States       => Finite_Node_Sets.Lattice,
         Confluence   => Dataflow.Must,
         Flow         => Dataflow.Forward,
         Visit_Assign => Handle_Assignment,
         Visit_Ignore => Remove_Possibly_Aliased,
         Entry_State  => Node_Sets.Empty_Set);

   end Ownership_Analysis;

   package body Ownership_Analysis is
      use Langkit_Support.Text;

      function Possibly_Aliased
        (Obj  : LAL.Defining_Name;
         Expr : LAL.Expr) return Boolean
      is
      begin
         for Ref of Obj.P_Find_Refs (Expr, LAL.No_Ada_Node) loop
            if LAL.Ref (Ref).Parent.Kind not in LALCO.Ada_Explicit_Deref then
               return True;
            end if;
         end loop;
         return False;
      end Possibly_Aliased;

      procedure Remove_Possibly_Aliased
        (State : in out Node_Sets.Set;
         Expr  : LAL.Expr)
      is
         use Node_Sets;

         C : Cursor := State.First;
         N : Cursor;
      begin
         while C /= No_Element loop
            N := Next (C);
            if Possibly_Aliased (Element (C).As_Defining_Name, Expr) then
               State.Delete (C);
            end if;
            C := N;
         end loop;
      end Remove_Possibly_Aliased;

      procedure Handle_Assignment
        (State : in out Node_Sets.Set;
         Dest  : LAL.Name;
         Val   : LAL.Expr)
      is
         use all type LAL.Ada_Node;

         D : LAL.Defining_Name :=
           (if Dest.P_Is_Defining
            then Dest.P_Enclosing_Defining_Name
            else Dest.P_Referenced_Defining_Name);
      begin
         Remove_Possibly_Aliased (State, Val);

         if Utils.Enclosing_Subp_Body (D) /= Target then
            return;
         end if;

         case Val.Kind is
            when LALCO.Ada_Allocator =>
               State.Include (D.As_Ada_Node);
            when others =>
               State.Exclude (D.As_Ada_Node);
         end case;
      end Handle_Assignment;

   end Ownership_Analysis;

   function Is_Owner_At
     (Var   : Libadalang.Analysis.Defining_Name;
      Place : Libadalang.Analysis.Ada_Node'Class) return Boolean
   is
      use all type LAL.Ada_Node;

      Subp : constant LAL.Base_Subp_Body := Utils.Enclosing_Subp_Body (Place);
   begin
      if Utils.Enclosing_Subp_Body (Var) /= Subp then
         raise Program_Error with "Conflicting subprograms for query";
      end if;
      declare
         package Subp_Analysis is new Ownership_Analysis (Subp);
         package Subp_Ownership renames Subp_Analysis.Ownership_Problem;

         Result : constant Subp_Ownership.Solution :=
            Subp_Ownership.Fixpoint (Subp);
      begin
         return Result.Query_At (Place.As_Ada_Node).Contains (Var.As_Ada_Node);
      end;
   end Is_Owner_At;

   package body Shared_Analysis is

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
               Map_Ref.Replace_Element (Cursor, Default (Subp));

               Universal_Solutions_Holder.Insert (Key, Convert (Placeholder));

               declare
                  Result : Context_Solution := Analyze (Subp);
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
