with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

with Post_Actions;

with Add_With_Clauses;
with Handle_Temporaries;
with Extend_Return_Stmts;
with Nest_Declare_Blocks;
with Track_Roots;
with Generate_Types_Interfaces;
with Detect_Misplaced_Bodies;
with Output_Unit;

procedure AGC is
   package Slocs   renames Langkit_Support.Slocs;

   package Helpers renames Libadalang.Helpers;
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;
   package LALU    renames Libadalang.Unparsing;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   procedure Post_Process
     (Ctx : Helpers.App_Context; Jobs : Helpers.App_Job_Context_Array);

   package App is new Helpers.App
     (Name             => "AGC",
      Description      => "Garbage collection for Ada",
      Process_Unit     => Process_Unit,
      App_Post_Process => Post_Process);

   package Output_Dir is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--output-dir",
      Arg_Type    => XString,
      Convert     => GNATCOLL.Opt_Parse.Convert,
      Default_Val => Null_XString,
      Help        =>
         "The directory in which to save the transformed ada units.");

   To_Do : Post_Actions.Actions;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      use type LALCO.Ada_Node_Kind_Type;
   begin
      if Unit.Has_Diagnostics then
         Put_Line ("Invalid ada unit " & Unit.Get_Filename);
      else
         Add_With_Clauses (Job_Ctx, Unit);
         Handle_Temporaries (Job_Ctx, Unit);
         Extend_Return_Stmts (Job_Ctx, Unit);
         Nest_Declare_Blocks (Job_Ctx, Unit);
         Track_Roots (Job_Ctx, Unit);
         Generate_Types_Interfaces (Job_Ctx, Unit);
         Detect_Misplaced_Bodies (Job_Ctx, Unit, To_Do);
      end if;
   end Process_Unit;

   procedure Post_Process
     (Ctx : Helpers.App_Context; Jobs : Helpers.App_Job_Context_Array)
   is
      First_Context : LAL.Analysis_Context :=
         Jobs (Jobs'First).Analysis_Ctx;
   begin
      To_Do.Perform_Actions (First_Context);

      --  output units
      for Job_Ctx of Jobs loop
         for Unit of Job_Ctx.Units_Processed loop
            Output_Unit (Job_Ctx, Unit, Output_Dir.Get);
         end loop;
      end loop;
   end Post_Process;
begin
   App.Run;
end AGC;
