with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Hashes;

with Libadalang.Common;

with Analysis.Dataflow;
with Dot_Printer;
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
         function Handle_Call
           (Spec : LAL.Base_Formal_Param_Holder'Class)
            return LALCO.Visit_Status
         is
            Is_Subp_Access : Boolean :=
               Spec.Parent.Kind in LALCO.Ada_Access_To_Subp_Def;

            Called_Decl : LAL.Basic_Decl :=
              (if Is_Subp_Access
               then LAL.No_Basic_Decl
               else Spec.Parent.As_Basic_Decl);

            Called_Body : LAL.Body_Node :=
              (if Is_Subp_Access
               then LAL.No_Body_Node
               else Utils.Get_Body (Called_Decl));
         begin
            if Called_Decl.Is_Null then
               Self_Allocates := True;
            elsif Called_Body.Is_Null then
               --  An instantiation with a null body is probably an
               --  instantiation of Unchecked_Deallocation,
               --  Unchecked_Conversion, etc.
               if Called_Decl.Kind
                     in LALCO.Ada_Enum_Literal_Decl
                      | LALCO.Ada_Generic_Subp_Internal
               then
                  return LALCO.Into;
               else
                  Self_Allocates := True;
               end if;
            else
               Calls.Include
                 ((Called_Body,
                   To_Unbounded_Text
                     (Called_Body.P_Unique_Identifying_Name)));
            end if;
            return LALCO.Into;
         end Handle_Call;

         function Process_Node
           (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
         is
         begin
            case Node.Kind is
               when LALCO.Ada_Allocator =>
                  if Utils.Is_Managed (Node.As_Allocator.P_Expression_Type) then
                     Self_Allocates := True;
                  end if;

               when LALCO.Ada_Name =>
                  declare
                     Called_Spec : LAL.Base_Formal_Param_Holder'Class :=
                        Node.As_Name.P_Called_Subp_Spec;
                  begin
                     if not Called_Spec.Is_Null then
                        return Handle_Call (Called_Spec);
                     end if;
                  end;

               when others =>
                  null;
            end case;
            return LALCO.Into;
         exception
            when LALCO.Property_Error | LALCO.Precondition_Failure =>
               Trace
                 (Analysis_Trace,
                  "Abandonning analysis of " & LAL.Image (Target));
               Self_Allocates := True;
               return LALCO.Over;
         end Process_Node;
      begin
         Self_Allocates := False;
         Target.Traverse (Process_Node'Access);
      end Compute_Summary;

      procedure Get
        (Does_Allocate : out Boolean;
         Called_Subps  : out Subp_Sets.Set)
      is
      begin
         if not Is_Computed then
            Trace
              (Analysis_Trace,
               "Computing summary for " & LAL.Image (Target));
            Compute_Summary;
            Trace
              (Analysis_Trace,
               "Result for " & LAL.Image (Target)
               & " : " & Self_Allocates'Image);
            Is_Computed := True;
         else
            Trace (Analysis_Trace, "Reusing summary");
         end if;

         Does_Allocate := Self_Allocates;
         Called_Subps  := Calls;
      end Get;

      entry Get_Blocking
        (Does_Allocate : out Boolean;
         Called_Subps  : out Subp_Sets.Set)
         when Is_Computed
      is
      begin
         Trace (Analysis_Trace, "Reusing summary (potentially waited)");
         Does_Allocate := Self_Allocates;
         Called_Subps  := Calls;
      end Get_Blocking;

      procedure Set_Global_Allocates (Value : Boolean) is
      begin
         Global_Allocates := (if Value then True else False);
      end Set_Global_Allocates;

      procedure Get_Global_Allocates (Value : out Tristate) is
      begin
         Value := Global_Allocates;
      end Get_Global_Allocates;

      entry Get_Global_Allocates_Blocking
        (Value : out Boolean)
         when Global_Allocates /= Unknown
      is
      begin
         Value := Global_Allocates = True;
      end Get_Global_Allocates_Blocking;
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
            Trace
              (Analysis_Trace,
               "Summary for " & LAL.Image (Subprogram) & " already requested");
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

      procedure Dump_Existing_Call_Graph (Path : String) is
         use Langkit_Support.Text;

         Printer : Dot_Printer.Printer;

         function Split
           (FQN  : Unbounded_Text_Type;
            Name : out Unbounded_Text_Type)
            return Dot_Printer.Cluster_Path_Type
         is
            use Ada.Strings.Wide_Wide_Unbounded;
            use type Dot_Printer.Cluster_Path_Type;

            Dot_Index   : constant Natural := Index (FQN, ".");
            Blank_Index : constant Natural := Index (FQN, " ");
         begin
            if Dot_Index = 0 or Blank_Index = 0 or Dot_Index > Blank_Index then
               Name := FQN;
               return Dot_Printer.Empty_Path;
            else
               declare
                  Head : constant Unbounded_Text_Type :=
                     Unbounded_Slice (FQN, 1, Dot_Index - 1);

                  Tail : constant Unbounded_Text_Type :=
                     Unbounded_Slice (FQN, Dot_Index + 1, Length (FQN));
               begin
                  return (1 => Head) & Split (Tail, Name);
               end;
            end if;
         end Split;

         procedure Process_Summary (C : Local_Summaries.Cursor) is
            FQN     : constant Unbounded_Text_Type := Local_Summaries.Key (C);
            Summary : constant Summary_Access := Local_Summaries.Element (C);
            Id      : constant Ada.Containers.Hash_Type := Hash (FQN);

            Name         : Unbounded_Text_Type;
            Cluster_Path : constant Dot_Printer.Cluster_Path_Type :=
               Split (FQN, Name);

            Color : Unbounded_Text_Type;

            Self_Allocates : Boolean;
            Call_Allocates : Boolean;
            Calls          : Subp_Sets.Set;
         begin
            Summary.Get (Self_Allocates, Calls);
            Summary.Get_Global_Allocates_Blocking (Call_Allocates);

            if Self_Allocates then
               Color := To_Unbounded_Text ("red");
            elsif Call_Allocates then
               Color := To_Unbounded_Text ("blue");
            end if;

            Printer.Add_Node
              (Id      => Id,
               Name    => Name,
               Cluster => Cluster_Path,
               Color   => Color);

            for C of Calls loop
               Printer.Add_Edge (Id, Hash (C.Id));
            end loop;
         end Process_Summary;
      begin
         Map.Iterate (Process_Summary'Access);
         Printer.Save (Path);
      end Dump_Existing_Call_Graph;
   end Summaries_Map;

   function Does_Allocate
     (Subprogram : Libadalang.Analysis.Body_Node'Class) return Boolean
   is
      Visited : Subp_Sets.Set;
      Ctx : LAL.Analysis_Context := Subprogram.Unit.Context;

      function Recurse (Info : Subp_Info) return Boolean is
         Summary          : Summary_Access;
         Self_Allocates   : Boolean;
         Global_Allocates : Tristate;
         Called_Subps     : Subp_Sets.Set;
      begin
         if Info.Subp.Unit.Context /= Ctx then
            Summaries.Get_Existing_Summary (Info.Id, Summary);
            Summary.Get_Global_Allocates_Blocking (Self_Allocates);
            return Self_Allocates;
         end if;

         Summaries.Get_Summary (Info.Subp, Summary);
         Summary.Get_Global_Allocates (Global_Allocates);

         if Global_Allocates /= Unknown then
            return Global_Allocates = True;
         end if;

         Summary.Get (Self_Allocates, Called_Subps);

         if Self_Allocates then
            Summary.Set_Global_Allocates (True);
            return True;
         end if;

         Visited.Insert (Info);

         for Called_Info of Called_Subps loop
            if not Visited.Contains (Called_Info) then
               if Recurse (Called_Info) then
                  Summary.Set_Global_Allocates (True);
                  return True;
               end if;
            end if;
         end loop;

         Summary.Set_Global_Allocates (False);
         return False;
      end Recurse;
   begin
      return Recurse
        ((Subprogram.As_Body_Node,
          To_Unbounded_Text
            (Subprogram.P_Unique_Identifying_Name)));
   end Does_Allocate;

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Ada_Node, LAL.Hash, LAL."=", LAL."=");

   function Node_Image (X : LAL.Ada_Node) return String is
     (Langkit_Support.Text.Image (X.Text));

   package Finite_Node_Sets is new Dataflow.Finite_Sets
     (Node_Sets, Node_Image);

   generic
      Target : LAL.Base_Subp_Body;
   package Ownership_Analysis is

      procedure Remove_Possibly_Aliased
        (State : in out Node_Sets.Set;
         Expr  : LAL.Expr);

      procedure Handle_Assignment
        (State : in out Node_Sets.Set;
         Dest  : LAL.Name;
         Val   : LAL.Expr);

      package Ownership_Problem is new Dataflow.Problem
        (States       => Finite_Node_Sets.Lattice,
         Entry_State  => Node_Sets.Empty_Set,
         Visit_Assign => Handle_Assignment,
         Visit_Ignore => Remove_Possibly_Aliased);

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

   function Is_Owner_After
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
         State : constant Node_Sets.Set :=
            Result.Query_After (Place.As_Ada_Node);
      begin
         return State.Contains (Var.As_Ada_Node);
      end;
   end Is_Owner_After;
end Analysis;
