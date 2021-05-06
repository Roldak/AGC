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
      Ctx   : LAL.Base_Subp_Body;
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
