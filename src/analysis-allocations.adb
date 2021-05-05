with Ada.Text_IO; use Ada.Text_IO;
with Analysis.Call_Graph;
with Utils;

package body Analysis.Allocations is
   use type Unbounded_Text_Type;

   function Analyze (Subp : LAL.Body_Node) return Boolean is
      Result : Boolean := False;

      function Process_Node
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         case Node.Kind is
            when LALCO.Ada_Allocator =>
               if Utils.Is_Managed (Node.As_Allocator.P_Expression_Type) then
                  Result := True;
                  return LALCO.Stop;
               end if;

            when others =>
               null;
         end case;
         return LALCO.Into;
      exception
         when LALCO.Property_Error | LALCO.Precondition_Failure =>
            Trace
              (Analysis_Trace,
               "Abandonning analysis of " & LAL.Image (Subp));
            Result := True;
            return LALCO.Stop;
      end Process_Node;

      procedure Handle_Context_Solution
        (R : Call_Graph.Context_Solution)
      is
      begin
         if R.Has_Unknown_Calls then
            Result := True;
            return;
         end if;

         for Call of R.Known_Calls loop
            if Allocations.Share.Get_Universal_Solution
                 (Call.As_Body_Node)
            then
               Result := True;
               return;
            end if;
         end loop;
      end Handle_Context_Solution;

      function Try_Universal_Solution
        (R : Call_Graph.Universal_Solution) return Boolean
      is
         use type Ada.Containers.Count_Type;
      begin
         if R.Has_Unknown_Calls then
            Result := True;
            return True;
         elsif R.Known_Calls.Length = 0 then
            return True;
         else
            return False;
         end if;
      end Try_Universal_Solution;
   begin
      Subp.Traverse (Process_Node'Access);
      if not Result then
         Call_Graph.Share.Process_Any_Solution
           (Subp,
            Try_Universal_Solution'Access,
            Handle_Context_Solution'Access);
      end if;
      return Result;
   end Analyze;
end Analysis.Allocations;
