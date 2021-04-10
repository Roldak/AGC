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

   type Add_Generic_Formal_Action is record
      Unit : Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location;
      Fix  : Langkit_Support.Text.Unbounded_Text_Type;
   end record;

   type Add_Generic_Actual_Action is record
      Unit : Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location;
      Fix  : Langkit_Support.Text.Unbounded_Text_Type;
   end record;

   type Add_Basic_Decl_After_Action is record
      Unit : Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location;
      Fix  : Langkit_Support.Text.Unbounded_Text_Type;
   end record;

   package Move_Actions is new Ada.Containers.Vectors
     (Positive, Move_Action);

   package Generate_External_Interface_Actions is new Ada.Containers.Vectors
     (Positive, Generate_External_Interface_Action);

   package Add_With_Clause_Actions is new Ada.Containers.Vectors
     (Positive, Add_With_Clause_Action);

   package Add_Generic_Formal_Actions is new Ada.Containers.Vectors
     (Positive, Add_Generic_Formal_Action);

   package Add_Generic_Actual_Actions is new Ada.Containers.Vectors
     (Positive, Add_Generic_Actual_Action);

   package Add_Basic_Decl_Actions is new Ada.Containers.Vectors
     (Positive, Add_Basic_Decl_After_Action);

   protected Actions is
      procedure Move (Action : Move_Action);
      procedure Add_With_Clause (Action : Add_With_Clause_Action);
      procedure Add_Generic_Formal (Action : Add_Generic_Formal_Action);
      procedure Add_Generic_Actual (Action : Add_Generic_Actual_Action);
      procedure Add_Basic_Decl_After (Action : Add_Basic_Decl_After_Action);
      procedure Generate_External_Interface
        (Action : Generate_External_Interface_Action);

      procedure Perform_Actions
        (Ctx   : Analysis_Context;
         Units : in out Libadalang.Helpers.Unit_Vectors.Vector);

      function Length return Natural;
   private
      To_Move        : Move_Actions.Vector;
      To_Generate    : Generate_External_Interface_Actions.Vector;
      To_With        : Add_With_Clause_Actions.Vector;
      Formals_To_Add : Add_Generic_Formal_Actions.Vector;
      Actuals_To_Add : Add_Generic_Actual_Actions.Vector;
      Decls_To_Add   : Add_Basic_Decl_Actions.Vector;
   end Actions;
end Post_Actions;
