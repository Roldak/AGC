with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Hashes;

with Libadalang.Common;

with Analysis.Dataflow;
with Analysis.Lattices.Finite_Node_Sets;
with Dot_Printer;
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

      function Get_Or_Compute
        (Subp : LAL.Base_Subp_Body) return Context_Solution
      is
         Key : constant Key_Type :=
            To_Unbounded_Text (Subp.P_Unique_Identifying_Name);

         Ctx : constant LAL.Analysis_Context := Subp.Unit.Context;

         Result       : Context_Solution;
         Summary      : Summary_Access;
         Must_Compute : Boolean;
      begin
         Holder.Get_Access_To_Summary (Key, Ctx, Summary, Must_Compute);

         if Must_Compute then
            --  Summary was created for us, analyze the subprogram
            Result := Analyze (Subp);
            Summary.Set (Result);
         elsif Summary.Get_Computer = Ctx then
            --  Summary already exists, and we are assigned to compute it.
            --  This means recursion, so return the default value.
            return Default (Subp);
         elsif Summary.Get_Computer /= LAL.No_Analysis_Context then
            --  Summary already exists and we are not the assignee. Duplicate
            --  the analysis in order to avoid a dead-lock in case of
            --  mutual recursion.
            --  TODO: Find a better solution.
            Result := Analyze (Subp);
            Summary.Set (Result);
         else
            --  Summary already exists and there is not assignee, which means
            --  it is ready to be retrieve and used.
            Summary.Get (Result);
         end if;
         return Result;
      end Get_Or_Compute;

      function Get_Or_Return
        (Subp_Name   : Unbounded_Text_Type;
         Result      : out Context_Solution) return Boolean
      is
         Summary : Summary_Access :=
            Holder.Get_Access_To_Existing_Summary (Subp_Name);
      begin
         if Summary = null then
            return False;
         end if;
         Summary.Get (Result);
         return True;
      end Get_Or_Return;

      protected body Summary_Type is
         procedure Seize (Ctx : LAL.Analysis_Context) is
         begin
            Computer := Ctx;
         end Seize;

         procedure Set (R : Context_Solution) is
         begin
            Result   := R;
            Computer := LAL.No_Analysis_Context;
         end Set;

         entry Get (R : out Context_Solution)
            when Computer = LAL.No_Analysis_Context
         is
         begin
            R := Result;
         end Get;

         function Get_Computer return LAL.Analysis_Context is
         begin
            return Computer;
         end Get_Computer;
      end Summary_Type;

      protected body Holder is
         function Get_Access_To_Existing_Summary
           (Subp : Key_Type) return Summary_Access
         is
            use type Cache_Maps.Cursor;

            Cursor : Cache_Maps.Cursor := Cache.Find (Subp);
         begin
            if Cursor = Cache_Maps.No_Element then
               return null;
            end if;
            return Cache_Maps.Element (Cursor);
         end Get_Access_To_Existing_Summary;

         procedure Get_Access_To_Summary
           (Subp         : Key_Type;
            Ctx          : LAL.Analysis_Context;
            Summary      : out Summary_Access;
            Must_Compute : out Boolean)
         is
            Cursor : Cache_Maps.Cursor;
         begin
            Cache.Insert (Subp, Cursor, Must_Compute);
            if Must_Compute then
               Summary := new Summary_Type;
               Summary.Seize (Ctx);
               Cache.Replace_Element (Cursor, Summary);
            else
               Summary := Cache_Maps.Element (Cursor);
            end if;
         end Get_Access_To_Summary;
      end Holder;

   end Shared_Analysis;
end Analysis;
