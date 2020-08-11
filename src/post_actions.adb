with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Rewriting;

with Node_Counters;
with Utils;

package body Post_Actions is
   package LAL   renames Libadalang.Analysis;
   package LALRW renames Libadalang.Rewriting;

   Insertions : Node_Counters.Counter;

   protected body Actions is
      procedure Register (Action : Move_Action) is
      begin
         To_Move.Append (Action);
      end Register;

      procedure Perform_Actions (Ctx : LAL.Analysis_Context) is
         RH : LALRW.Rewriting_Handle :=
            LALRW.Start_Rewriting (Ctx);
      begin
         for Action of To_Move loop
            declare
               Source : LAL.Ada_Node := Action.Source;
               Dest   : LAL.Ada_Node := Action.Dest;
               Index  : Natural      := Node_Counters.Get (Insertions, Dest);
            begin
               LALRW.Remove_Child
                 (LALRW.Handle (Source.Parent),
                  Utils.Child_Index (LALRW.Handle (Source)));
               LALRW.Insert_Child
                 (LALRW.Handle (Dest), Index + 1, LALRW.Handle (Source));

               Node_Counters.Increase (Insertions, Dest);
            end;
         end loop;

         if not LALRW.Apply (RH).Success then
            raise Program_Error
               with "post_actions: could not apply rewritings";
         end if;
      end Perform_Actions;
   end Actions;
end Post_Actions;
