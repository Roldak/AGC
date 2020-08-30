with Ada.Containers.Vectors;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;

package Post_Actions is
   type Move_Action is record
      Unit : Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location;
   end record;

   type Generate_External_Interface_Action is record
      Unit : Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location;
   end record;

   type Add_With_Clause_Action is record
      Unit : Analysis_Unit;
      Ref  : Langkit_Support.Text.Unbounded_Text_Type;
   end record;

   package Move_Actions is new Ada.Containers.Vectors
     (Positive, Move_Action);

   package Generate_External_Interface_Actions is new Ada.Containers.Vectors
     (Positive, Generate_External_Interface_Action);

   package Add_With_Clause_Actions is new Ada.Containers.Vectors
     (Positive, Add_With_Clause_Action);

   protected type Actions is
      procedure Register (Action : Move_Action);
      procedure Register (Action : Generate_External_Interface_Action);
      procedure Register (Action : Add_With_Clause_Action);

      procedure Perform_Actions
        (Ctx   : Analysis_Context;
         Units : in out Libadalang.Helpers.Unit_Vectors.Vector);
   private
      To_Move     : Move_Actions.Vector;
      To_Generate : Generate_External_Interface_Actions.Vector;
      To_With     : Add_With_Clause_Actions.Vector;
   end Actions;
end Post_Actions;
