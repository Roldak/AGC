with Utils;

package body Analysis.Ownership is
   use Langkit_Support.Text;

   function Analyze (X : LAL.Body_Node) return Problem.Solution is
   begin
      return Problem.Fixpoint (X.As_Base_Subp_Body);
   end Analyze;

   function Default (X : LAL.Body_Node) return Problem.Solution is
      R : Problem.Solution;
   begin
      return R;
   end Default;

   procedure Include_Parameter
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Param : LAL.Param_Spec)
   is
   begin
      if not Utils.Is_Managed (Param.P_Formal_Type) then
         return;
      end if;

      for Id of Param.F_Ids loop
         State.Include (Id.As_Ada_Node);
      end loop;
   end Include_Parameter;

   function Is_Aliasing_Reference (Id : LAL.Base_Id'Class) return Boolean is
   begin
      case Id.Parent.Kind is
         when LALCO.Ada_Explicit_Deref =>
            return False;
         when LALCO.Ada_Param_Assoc =>
            declare
               Param_Assoc : constant LAL.Param_Assoc :=
                  Id.Parent.As_Param_Assoc;

               Param_Name  : constant LAL.Defining_Name :=
                  Param_Assoc.P_Get_Params (1);

               Param_Spec  : constant LAL.Param_Spec :=
                  Param_Name.P_Basic_Decl.As_Param_Spec;

               Called_Decl : constant LAL.Basic_Decl :=
                  Param_Spec.P_Semantic_Parent.As_Basic_Decl;

               Called_Body : constant LAL.Body_Node :=
                  Utils.Get_Body (Called_Decl);

               Callee_Solution : constant Problem.Solution :=
                  Ownership.Share.Get_Context_Solution
                    (Called_Body);

               Is_Still_Owner : Boolean := True;

               procedure Contains_Param (X : LAL.Ada_Node; S : Node_Sets.Set) is
               begin
                  if not S.Contains (Param_Name.As_Ada_Node) then
                     Is_Still_Owner := False;
                  end if;
               end Contains_Param;
            begin
               Callee_Solution.Query_End_States
                 (Contains_Param'Access);
               return not Is_Still_Owner;
            exception
               when LALCO.Property_Error | LALCO.Precondition_Failure =>
                  Trace
                    (Analysis_Trace,
                     "Abandonning Is_Aliasing_Reference of " & LAL.Image (Id));
                  return True;
            end;
         when others =>
            return True;
      end case;
   end Is_Aliasing_Reference;

   function Is_Aliased_In
     (Obj  : LAL.Defining_Name;
      Expr : LAL.Expr) return Boolean
   is
   begin
      for Ref of Obj.P_Find_Refs (Expr, LAL.No_Ada_Node) loop
         if Is_Aliasing_Reference (LAL.Ref (Ref)) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Aliased_In;

   procedure Remove_Possibly_Aliased
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Expr  : LAL.Expr)
   is
      use Node_Sets;

      C : Cursor := State.First;
      N : Cursor;
   begin
      while C /= No_Element loop
         N := Next (C);
         if Is_Aliased_In (Element (C).As_Defining_Name, Expr) then
            State.Delete (C);
         end if;
         C := N;
      end loop;
   end Remove_Possibly_Aliased;

   procedure Handle_Assignment
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Dest  : LAL.Name;
      Val   : LAL.Expr)
   is
      use all type LAL.Ada_Node;

      D : LAL.Defining_Name :=
        (if Dest.P_Is_Defining
         then Dest.P_Enclosing_Defining_Name
         else Dest.P_Referenced_Defining_Name);
   begin
      Remove_Possibly_Aliased (State, Ctx, Val);

      if Utils.Enclosing_Subp_Body (D) /= Ctx then
         return;
      end if;

      case Val.Kind is
         when LALCO.Ada_Allocator =>
            State.Include (D.As_Ada_Node);
         when others =>
            State.Exclude (D.As_Ada_Node);
      end case;
   end Handle_Assignment;
end Analysis.Ownership;
