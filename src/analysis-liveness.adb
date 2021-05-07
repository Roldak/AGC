with Utils;

package body Analysis.Liveness is
   use Langkit_Support.Text;
   use all type LAL.Ada_Node;

   function Analyze (X : LAL.Body_Node) return Problem.Solution is
   begin
      return Problem.Fixpoint (X.As_Base_Subp_Body);
   end Analyze;

   function Default (X : LAL.Body_Node) return Problem.Solution is
      R : Problem.Solution;
   begin
      return R;
   end Default;

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
                  if Utils.Defined_In_Subp (Ctx, Ref) then
                     State.Include (Ref.P_Canonical_Part.As_Ada_Node);
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
      D : constant LAL.Defining_Name :=
        (if Dest.P_Is_Defining
         then Dest.P_Enclosing_Defining_Name
         else Dest.P_Referenced_Defining_Name.P_Canonical_Part);
   begin
      Add_All_References (State, Ctx, Val);

      if Utils.Defined_In_Subp (Ctx, D) then
         State.Exclude (D.As_Ada_Node);
      end if;
   end Handle_Assignment;
end Analysis.Liveness;
