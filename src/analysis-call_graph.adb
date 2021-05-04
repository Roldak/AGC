with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Utils;

package body Analysis.Call_Graph is
   function Analyze (Subp : LAL.Base_Subp_Body) return Context_Solution is
      Result : Context_Solution;

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
            Result.Has_Unknown_Calls := True;
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
               Result.Has_Unknown_Calls := True;
            end if;
         else
            Result.Known_Calls.Include (Called_Body.As_Ada_Node);
         end if;
         return LALCO.Into;
      end Handle_Call;

      function Process_Node
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         case Node.Kind is
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
            Result.Has_Unknown_Calls := True;
            Trace
              (Analysis_Trace,
               "Error while analyzing " & LAL.Image (Subp));
            return LALCO.Over;
      end Process_Node;
   begin
      Result.Has_Unknown_Calls := False;
      Subp.Traverse (Process_Node'Access);
      return Result;
   end Analyze;

   function To_Universal (X : Context_Solution) return Universal_Solution is
      Result : Universal_Solution;
   begin
      Result.Has_Unknown_Calls := X.Has_Unknown_Calls;
      for Call of X.Known_Calls loop
         Result.Known_Calls.Include
           (To_Unbounded_Text
              (Call.As_Base_Subp_Body.P_Unique_Identifying_Name));
      end loop;
      return Result;
   end To_Universal;
end Analysis.Call_Graph;
