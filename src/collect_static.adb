with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Iterators;
with Libadalang.Rewriting;

with Analysis;
with Analysis.Lattices.Finite_Node_Sets;
with Analysis.Dataflow;
with Analysis.Ownership;
with Session;
with Utils;

procedure Collect_Static
  (Job_Ctx  : Libadalang.Helpers.App_Job_Context;
   Unit     : Libadalang.Analysis.Analysis_Unit)
is
   use type Session.Optimization_Level_Type;

   package LAL     renames Libadalang.Analysis;
   package LALI    renames Libadalang.Iterators;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   package Node_Sets renames Analysis.Lattices.Finite_Node_Sets.Node_Sets;

   use all type LAL.Ada_Node;

   --  Rewriting handle is created lazily in this phase because many units
   --  won't need to be rewritten.
   RH : LALRW.Rewriting_Handle := LALRW.No_Rewriting_Handle;

   procedure Start_Rewriting is
      use type LALRW.Rewriting_Handle;
   begin
      if RH = LALRW.No_Rewriting_Handle then
         RH := LALRW.Start_Rewriting (Unit.Context);
      end if;
   end Start_Rewriting;

   function Apply_Rewritings_If_Relevant return Boolean is
      use type LALRW.Rewriting_Handle;
   begin
      if RH /= LALRW.No_Rewriting_Handle then
         return LALRW.Apply (RH).Success;
      end if;
      return True;
   end Apply_Rewritings_If_Relevant;

   procedure Kill
     (Subp     : LAL.Base_Subp_Body;
      Location : LAL.Ada_Node;
      Var      : LAL.Defining_Name)
   is
      use type LAL.Ada_Node;
      use LALI;

      Best_Location : LAL.Ada_Node :=
        (if Location.Parent.Kind in LALCO.Ada_Stmt_List
         then Location.Parent
         else Find_First (Location, Kind_Is (LALCO.Ada_Stmt_List)));

      Index : Positive;

      Is_Owner : Boolean :=
         Analysis.Ownership.Share.Get_Context_Solution (Subp.As_Body_Node)
         .Query_At (Location.As_Ada_Node).Contains (Var.As_Ada_Node);
   begin
      if not Best_Location.Is_Null and then Is_Owner then
         Start_Rewriting;

         if Best_Location = Location.Parent then
            Index := Utils.Child_Index (LALRW.Handle (Location)) + 1;
         else
            Index := 1;
         end if;

         LALRW.Insert_Child
           (LALRW.Handle (Best_Location),
            Index,
            LALRW.Create_From_Template
              (RH, "AGC.Free (" & Var.Text & ");",
               (1 .. 0 => <>), LALCO.Stmt_Rule));
      end if;
   end Kill;

   procedure Add_All_References
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Expr  : LAL.Expr)
   is
      function Add_Ref (X : LAL.Ada_Node'Class) return LALCO.Visit_Status is
         Ref : LAL.Defining_Name;
      begin
         case X.Kind is
            when LALCO.Ada_Identifier =>
               Ref := X.As_Name.P_Referenced_Defining_Name;
               if not Ref.Is_Null then
                  if Utils.Enclosing_Subp_Body (Ref) = Ctx then
                     State.Include (Ref.As_Ada_Node);
                  end if;
               end if;
            when others =>
               null;
         end case;
         return LALCO.Into;
      exception
         when LALCO.Property_Error =>
            return LALCO.Over;
      end Add_Ref;
   begin
      Expr.Traverse (Add_Ref'Access);
   end Add_All_References;

   procedure Handle_Assignment
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Dest  : LAL.Name;
      Val   : LAL.Expr)
   is
      D : LAL.Defining_Name :=
        (if Dest.P_Is_Defining
         then Dest.P_Enclosing_Defining_Name
         else Dest.P_Referenced_Defining_Name);
   begin
      Add_All_References (State, Ctx, Val);

      if Utils.Enclosing_Subp_Body (D) = Ctx then
         State.Exclude (D.As_Ada_Node);
      end if;
   end Handle_Assignment;

   package Liveness_Problem is new Analysis.Dataflow.Problem
     (States       => Analysis.Lattices.Finite_Node_Sets.Lattice,
      Confluence   => Analysis.Dataflow.May,
      Flow         => Analysis.Dataflow.Backwards,
      Visit_Assign => Handle_Assignment,
      Visit_Ignore => Add_All_References,
      Entry_State  => Node_Sets.Empty_Set);

   procedure Handle_Subp_Body (Subp : LAL.Base_Subp_Body) is
      Result : Liveness_Problem.Solution :=
         Liveness_Problem.Fixpoint (Subp);

      procedure Detect_Cause_Of_Death
        (N : LAL.Ada_Node; S : Node_Sets.Set)
      is
         use Analysis.Lattices;

         Total : Node_Sets.Set;

         procedure Compare_To (M : LAL.Ada_Node; R : Node_Sets.Set) is
            use Finite_Node_Sets.Lattice;
         begin
            if not Leq (R, S) then
               Total := Total.Union (R.Difference (S));
            end if;
         end Compare_To;
      begin
         Result.Query_Before (N, Compare_To'Access);

         for Var of Total loop
            Kill (Subp, N, Var.As_Defining_Name);
         end loop;
      end Detect_Cause_Of_Death;
   begin
      Result.Iterate (Detect_Cause_Of_Death'Access);
   end Handle_Subp_Body;

   function Process_Node
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Base_Subp_Body =>
            Handle_Subp_Body (Node.As_Base_Subp_Body);
         when others =>
            null;
      end case;
      return LALCO.Into;
   end Process_Node;
begin
   if Session.Get_Optimization_Level /= Session.Full then
      return;
   end if;

   Unit.Root.Traverse (Process_Node'Access);
   if not Apply_Rewritings_If_Relevant then
      raise Program_Error with "collect_static: could not apply rewritings";
   end if;
end Collect_Static;
