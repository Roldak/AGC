with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Analysis.Call_Graph;
with Utils;

package body Analysis.Allocations is
   use type Unbounded_Text_Type;

   function Analyze (Subp : LAL.Base_Subp_Body) return Boolean is
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
   begin
      Subp.Traverse (Process_Node'Access);
      if Result then
         return True;
      end if;
      declare
         Call_Summary : Call_Graph.Context_Solution :=
            Call_Graph.Share.Get_Or_Compute (Subp);
      begin
         if Call_Summary.Has_Unknown_Calls then
            return True;
         end if;

         for Call of Call_Summary.Known_Calls loop
            if Allocations.Share.Get_Or_Compute (Call.As_Base_Subp_Body) then
               return True;
            end if;
         end loop;

         return False;
      end;
   end Analyze;
end Analysis.Allocations;
