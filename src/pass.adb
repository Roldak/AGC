package body Pass is
   function Rewriting_Handle
     (Unit : LAL.Analysis_Unit) return LALRW.Rewriting_Handle
   is
      use type LALRW.Rewriting_Handle;

      Ctx      : constant Libadalang.Analysis.Analysis_Context := Unit.Context;
      Existing : constant LALRW.Rewriting_Handle := LALRW.Handle (Ctx);
   begin
      if Existing /= LALRW.No_Rewriting_Handle then
         return Existing;
      else
         return LALRW.Start_Rewriting (Ctx);
      end if;
   end Rewriting_Handle;

   procedure Apply_Rewritings
     (Unit       : LAL.Analysis_Unit;
      Error_Name : String)
   is
      use type LALRW.Rewriting_Handle;

      RH : LALRW.Rewriting_Handle := LALRW.Handle (Unit.Context);
   begin
      if RH /= LALRW.No_Rewriting_Handle then
         if not LALRW.Apply (RH).Success then
            raise Program_Error with
               Error_Name & ": could not apply rewritings";
         end if;
      end if;
   end Apply_Rewritings;
end Pass;
