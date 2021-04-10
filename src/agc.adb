with Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

with Analysis;
with Post_Actions;
with Session;

with Add_With_Clauses;
with Unsugar_Expr_Functions;
with Handle_Temporaries;
with Extend_Return_Stmts;
with Track_Roots;
with Generate_Types_Interfaces;
with Register_Global_Changes;
with Output_Unit;

procedure AGC is
   package Slocs   renames Langkit_Support.Slocs;

   package Helpers renames Libadalang.Helpers;
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;
   package LALU    renames Libadalang.Unparsing;

   procedure Setup
     (Ctx   : Helpers.App_Context;
      Jobs  : Helpers.App_Job_Context_Array;
      Files : Helpers.String_Vectors.Vector);

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   procedure Post_Process
     (Ctx : Helpers.App_Context; Jobs : Helpers.App_Job_Context_Array);

   package App is new Helpers.App
     (Name               => "AGC",
      Description        => "Garbage collection for Ada",
      App_Setup          => Setup,
      Process_Unit       => Process_Unit,
      App_Post_Process   => Post_Process,
      Enable_Parallelism => True);

   package Output_Dir is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--output-dir",
      Arg_Type    => XString,
      Convert     => GNATCOLL.Opt_Parse.Convert,
      Default_Val => Null_XString,
      Help        =>
         "The directory in which to save the transformed ada units.");

   package Optimize is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser  => App.Args.Parser,
      Long    => "--optimize",
      Help    => "Turn on optimizations");

   package Quiet is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Short  => "-q",
      Long   => "--quiet",
      Help   => "Do not print progress information");

   procedure Put_Line (X : String) is
   begin
      if Quiet.Get then
         return;
      end if;
      Ada.Text_IO.Put_Line (X);
   end Put_Line;

   procedure Print_Processed_Unit (Unit : LAL.Analysis_Unit) is
      Basename : constant String := +Create (+Unit.Get_Filename).Base_Name;
   begin
      Put_Line ("   [Ada]          " & Basename);
   end Print_Processed_Unit;

   procedure Setup
     (Ctx   : Helpers.App_Context;
      Jobs  : Helpers.App_Job_Context_Array;
      Files : Helpers.String_Vectors.Vector)
   is
   begin
      Put_Line ("Instrument");
      Session.Set_Files_To_Process (Files);
      if Optimize.Get then
         Analysis.Summaries := new Analysis.Summaries_Map;
      end if;
   end Setup;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      use type LALCO.Ada_Node_Kind_Type;
   begin
      Print_Processed_Unit (Unit);
      if Unit.Has_Diagnostics then
         Put_Line ("Invalid ada unit " & Unit.Get_Filename);
      else
         Add_With_Clauses (Job_Ctx, Unit);
         Unsugar_Expr_Functions (Job_Ctx, Unit);
         Handle_Temporaries (Job_Ctx, Unit);
         Extend_Return_Stmts (Job_Ctx, Unit);
         Track_Roots (Job_Ctx, Unit);
         Generate_Types_Interfaces (Job_Ctx, Unit);
         Register_Global_Changes (Job_Ctx, Unit);
      end if;
   end Process_Unit;

   procedure Reparse_All_Units
     (Dest      : LAL.Analysis_Context;
      Jobs      : Helpers.App_Job_Context_Array;
      All_Units : in out Helpers.Unit_Vectors.Vector)
   is
   begin
      for Job_Ctx of Jobs loop
         for Unit of Job_Ctx.Units_Processed loop
            declare
               Reparsed : LAL.Analysis_Unit := LAL.Get_From_Buffer
                 (Context  => Dest,
                  Filename => LAL.Get_Filename (Unit),
                  Buffer   => Langkit_Support.Text.Encode
                    (LAL.Text (Unit),
                     LAL.Get_Charset (Unit)));
            begin
               All_Units.Append (Reparsed);
            end;
         end loop;
      end loop;
   end Reparse_All_Units;

   procedure Post_Process
     (Ctx : Helpers.App_Context; Jobs : Helpers.App_Job_Context_Array)
   is
      use type Helpers.Job_ID;

      First_Context : LAL.Analysis_Context :=
         Jobs (Jobs'First).Analysis_Ctx;

      All_Units : Helpers.Unit_Vectors.Vector :=
         Jobs (Jobs'First).Units_Processed;
   begin
      Put_Line ("Apply" & Session.To_Do.Length'Image & " Global Changes");

      Reparse_All_Units
        (First_Context,
         Jobs (Jobs'First + 1 .. Jobs'Last),
         All_Units);

      Session.To_Do.Perform_Actions (First_Context, All_Units);

      Put_Line ("Output" & All_Units.Length'Image & " Units");

      --  output units
      for Unit of All_Units loop
         Output_Unit (Unit, Output_Dir.Get);
      end loop;

      Put_Line ("Done");
   end Post_Process;
begin
   App.Run;
end AGC;
