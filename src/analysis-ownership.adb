with Utils;

package body Analysis.Ownership is
   use Langkit_Support.Text;

   function Analyze (X : LAL.Body_Node) return Problem.Solution is
     (Problem.Fixpoint (X.As_Base_Subp_Body));

   function Default (X : LAL.Body_Node) return Problem.Solution is
     (Problem.Empty (X.As_Base_Subp_Body));

   procedure Include_Parameter
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Param : LAL.Param_Spec)
   is
   begin
      if Utils.Is_Managed (Param.P_Formal_Type) then
         for Id of Param.F_Ids loop
            State.Include (Id.P_Canonical_Part.As_Ada_Node);
         end loop;
      end if;
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

               Callee_Solution : constant Universal_Solution :=
                  Ownership.Share.Get_Universal_Solution
                    (Called_Body);
            begin
               return not Callee_Solution.Final_Owners.Contains
                 (To_Unbounded_Text (Param_Name.Text));
            end;
         when LALCO.Ada_Bin_Op_Range =>
            return not Id.Parent.As_Bin_Op.F_Op.P_Referenced_Decl.Is_Null;
         when others =>
            return True;
      end case;
   exception
      when LALCO.Property_Error | LALCO.Precondition_Failure =>
         Trace
           (Analysis_Trace,
            "Abandonning Is_Aliasing_Reference of " & LAL.Image (Id));
         return True;
   end Is_Aliasing_Reference;

   function Is_Aliased_In
     (Obj  : LAL.Defining_Name;
      Expr : LAL.Expr) return Boolean
   is
   begin
      for Ref of Obj.P_Find_Refs (Expr) loop
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

   function Returns_Owning_Access (E : LAL.Expr) return Boolean is
   begin
      if E.Kind in LALCO.Ada_Allocator then
         return True;
      elsif E.Kind in LALCO.Ada_Name and then E.As_Name.P_Is_Call then
         declare
            Name : constant LAL.Name := E.As_Name;

            Spec : constant LAL.Base_Formal_Param_Holder'Class :=
               Name.P_Called_Subp_Spec;

            Is_Subp_Access : constant Boolean :=
               Spec.Parent.Kind in LALCO.Ada_Access_To_Subp_Def;

            Called_Decl : constant LAL.Basic_Decl :=
              (if Is_Subp_Access
               then LAL.No_Basic_Decl
               else Spec.Parent.As_Basic_Decl);

            Called_Body : constant LAL.Body_Node :=
              (if Is_Subp_Access
               then LAL.No_Body_Node
               else Utils.Get_Body (Called_Decl));
         begin
            if not Called_Body.Is_Null then
               return Ownership.Share.Get_Universal_Solution
                 (Called_Body).Returns_Owner;
            end if;
         end;
      end if;
      return False;
   end Returns_Owning_Access;

   procedure Handle_Assignment
     (State : in out Node_Sets.Set;
      Ctx   : LAL.Base_Subp_Body;
      Dest  : LAL.Name;
      Val   : LAL.Expr)
   is
      use all type LAL.Ada_Node;

      D : constant LAL.Defining_Name :=
        (if Dest.P_Is_Defining
         then Dest.P_Enclosing_Defining_Name
         else Dest.P_Referenced_Defining_Name.P_Canonical_Part);
   begin
      Remove_Possibly_Aliased (State, Ctx, Val);

      if not Utils.Defined_In_Subp (Ctx, D) then
         return;
      end if;

      if Returns_Owning_Access (Val) then
         State.Include (D.As_Ada_Node);
      else
         State.Exclude (D.As_Ada_Node);
      end if;
   end Handle_Assignment;

   function To_Universal
     (X : Problem.Solution) return Universal_Solution
   is
      Result : Universal_Solution := (Returns_Owner => False, others => <>);
      First  : Boolean := True;

      procedure Handle_End_State (N : LAL.Ada_Node; S : Node_Sets.Set) is
         Temp : Id_Sets.Set;
      begin
         for N of S loop
            if N.As_Defining_Name.P_Basic_Decl.Kind in LALCO.Ada_Param_Spec then
               Temp.Insert (To_Unbounded_Text (N.Text));
            end if;
         end loop;

         Result.Final_Owners :=
           (if First then Temp else Result.Final_Owners.Intersection (Temp));

         Result.Returns_Owner :=
           (First or else Result.Returns_Owner) and then
           (case N.Kind is
               when LALCO.Ada_Return_Stmt =>
                  not N.As_Return_Stmt.F_Return_Expr.Is_Null and then
                  Returns_Owning_Access (N.As_Return_Stmt.F_Return_Expr),
               when LALCO.Ada_Extended_Return_Stmt =>
                  S.Contains
                    (N.As_Extended_Return_Stmt
                     .F_Decl.P_Defining_Name.As_Ada_Node),
               when others => False);

         First := False;
      end Handle_End_State;
   begin
      X.Query_End_States (Handle_End_State'Access);
      return Result;
   end To_Universal;

   function Is_Owner
     (Result : Universal_Solution;
      Param  : LAL.Defining_Name) return Boolean
   is
   begin
      return Result.Final_Owners.Contains (To_Unbounded_Text (Param.Text));
   end Is_Owner;
end Analysis.Ownership;
