with Ada.Containers.Vectors;

with Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;

package Post_Actions is
   type Move_Action is record
      Unit : Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location;
   end record;

   package Move_Actions is new Ada.Containers.Vectors
     (Positive, Move_Action);

   protected type Actions is
      procedure Register (Action : Move_Action);

      procedure Perform_Actions (Ctx : Analysis_Context);
   private
      To_Move : Move_Actions.Vector;
   end Actions;
end Post_Actions;
