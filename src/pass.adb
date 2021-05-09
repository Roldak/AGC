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
end Pass;
