with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Rewriting;
with Libadalang.Unparsing;

with Session;
with Utils;
with Wrapper;

with Analysis.Call_Graph.Dump;

with Pass.Check_Consistency;
with Pass.Add_With_Clauses;
with Pass.Unsugar_Expr_Functions;
with Pass.Handle_Temporaries;
with Pass.Collect_Static;
with Pass.Track_Roots;
with Pass.Generate_Types_Interfaces;
with Pass.Register_Global_Changes;
with Pass.Output_Unit;

with Post_Actions;

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
         "The directory in which to output the instrumented ada units."
       & "When invoked with a project file, this path is treated as "
       & "relative to the (sub-)projects' object directories, unless "
       & "this is an absolute path.");

   package Optimize is new GNATCOLL.Opt_Parse.Parse_Enum_Option
     (Parser      => App.Args.Parser,
      Long        => "--optimize",
      Help        => "Turn on optimizations",
      Arg_Type    => Session.Optimization_Level_Type,
      Default_Val => Session.None);

   package Quiet is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Short  => "-q",
      Long   => "--quiet",
      Help   => "Do not print progress information");

   package Before_GPRBuild is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Long   => "--before-gprbuild",
      Help   => "Print less information about instrumentation "
                & " for smoother integration with gprbuild's output.");

   package No_Hash is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Long   => "--no-hash",
      Help   => "Do not include the original unit's hash when emitting "
                & "an instrumented unit. This prevents incrementality.");

   package Force is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Short  => "-f",
      Long   => "--force",
      Help   => "Force (re-)instrumentation of all units");

   package Dump_Call_Graph is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--dump-call-graph",
      Arg_Type    => XString,
      Convert     => GNATCOLL.Opt_Parse.Convert,
      Default_Val => Null_XString,
      Help        =>
         "Dump the computed call-graph in a dot file to the given path. "
       & "This should be used with --optimize otherwise the computed "
       & "call-graph will probably be empty.");

   package No_Sanity_Check is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Long   => "--no-check",
      Help   => "Do not perform sanity checks (improve processing time)");

   procedure Put_Line (X : String; Always: Boolean := True) is
   begin
      if Quiet.Get or else (not Always and then Before_GPRBuild.Get) then
         return;
      end if;
      Ada.Text_IO.Put_Line (X);
   end Put_Line;

   procedure Print_Processed_Unit (Unit : LAL.Analysis_Unit) is
   begin
      Put_Line ("   [Ada]          " & Utils.Base_Name (Unit.Get_Filename));
   end Print_Processed_Unit;

   procedure Setup
     (Ctx   : Helpers.App_Context;
      Jobs  : Helpers.App_Job_Context_Array;
      Files : Helpers.String_Vectors.Vector)
   is
      use GNATCOLL;

      Out_Dir_File : VFS.Virtual_File := GNATCOLL.VFS.No_File;
   begin
      Traces.Parse_Config_File;

      if Output_Dir.Get /= "" then
         Out_Dir_File :=
            VFS.Create (VFS."+" (Strings.To_String (Output_Dir.Get)));
      end if;

      Session.Init_Session (Ctx, Out_Dir_File, Files, Optimize.Get);

      Put_Line ("Instrument");
   end Setup;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      use type LALCO.Ada_Node_Kind_Type;
   begin
      if not Force.Get and then not Session.Must_Reprocess (Unit) then
         return;
      end if;

      Print_Processed_Unit (Unit);

      if Unit.Has_Diagnostics then
         Put_Line ("Invalid ada unit " & Unit.Get_Filename);
      else
         Pass.Add_With_Clauses (Job_Ctx, Unit);
         Pass.Unsugar_Expr_Functions (Job_Ctx, Unit);
         Pass.Apply_Rewritings (Unit, "add_with_clause|unsugar_expr_functions");

         Pass.Handle_Temporaries (Job_Ctx, Unit);
         Pass.Apply_Rewritings (Unit, "handle_temporaries");

         Pass.Collect_Static (Job_Ctx, Unit);
         Pass.Apply_Rewritings (Unit, "collect_static");

         Pass.Track_Roots (Job_Ctx, Unit);
         Pass.Generate_Types_Interfaces (Job_Ctx, Unit);
         Pass.Apply_Rewritings (Unit, "track_roots|generate_type_interfaces");

         Pass.Register_Global_Changes (Job_Ctx, Unit);
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

   Done : Boolean := False;

   procedure Post_Process
     (Ctx : Helpers.App_Context; Jobs : Helpers.App_Job_Context_Array)
   is
      use type Helpers.Job_ID;

      First_Context : LAL.Analysis_Context :=
         Jobs (Jobs'First).Analysis_Ctx;

      All_Units : Helpers.Unit_Vectors.Vector :=
         Jobs (Jobs'First).Units_Processed;

      Total_Written : Natural := 0;

      Post_Action_Count : Natural := Post_Actions.Actions.Length;
   begin
      Reparse_All_Units
        (First_Context,
         Jobs (Jobs'First + 1 .. Jobs'Last),
         All_Units);

      if not No_Sanity_Check.Get then
         Put_Line ("Check consistency", False);

         if not Pass.Check_Consistency (Ctx, First_Context, All_Units) then
            return;
         end if;
      end if;

      if Post_Action_Count > 0 then
         Put_Line
           ("Apply" & Post_Action_Count'Image & " Global Changes", False);
      end if;

      Post_Actions.Actions.Perform_Actions (First_Context, All_Units);

      Put_Line ("Output units", False);

      --  output units
      for Unit of All_Units loop
         if Force.Get or else Session.Must_Reprocess (Unit) then
            Pass.Output_Unit (Unit => Unit, With_SHA1 => not No_Hash.Get);
            Total_Written := Total_Written + 1;
         end if;
      end loop;

      --  dump call graph
      if Dump_Call_Graph.Get /= "" then
         Put_Line ("Dump computed call-graph", False);
         Analysis.Call_Graph.Dump
           (GNATCOLL.Strings.To_String (Dump_Call_Graph.Get));
      end if;

      --  terminate
      if Total_Written = 0 then
         Put_Line ("Done: everything up-to-date.");
      else
         Put_Line
           ("Done: instrumented" & Total_Written'Image & " units.", False);
      end if;

      Post_Actions.Actions.Clear;
      Done := True;
   end Post_Process;
begin
   if not Wrapper then
      App.Run;
   end if;
exception
   when Program_Error =>
      if not Done then
         --  TODO: Fix finalization errors occurring during
         --  Libadalang.Helpers.Run.Finalize. In the meantime, do not crash.
         raise;
      end if;
end AGC;
