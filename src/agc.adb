with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Projects;
with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with GNAT.Sha1;
with GNAT.Strings;

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
with Utils;

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
         "The directory in which to output the instrumented ada units."
       & "When invoked with a project file, this path is treated as "
       & "relative to the (sub-)projects' object directories, unless "
       & "this is an absolute path.");

   package Optimize is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser  => App.Args.Parser,
      Long    => "--optimize",
      Help    => "Turn on optimizations");

   package Quiet is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Short  => "-q",
      Long   => "--quiet",
      Help   => "Do not print progress information");

   package No_Hash is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser => App.Args.Parser,
      Long   => "--no-hash",
      Help   => "Do not include the original unit's hash when emitting "
                & "an instrumented unit. This prevents incrementality.");

   procedure Put_Line (X : String) is
   begin
      if Quiet.Get then
         return;
      end if;
      Ada.Text_IO.Put_Line (X);
   end Put_Line;

   procedure Print_Processed_Unit (Unit : LAL.Analysis_Unit) is
   begin
      Put_Line ("   [Ada]          " & Utils.Base_Name (Unit.Get_Filename));
   end Print_Processed_Unit;

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Unbounded_String, Hash, "=");

   package String_Bool_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Boolean, Hash, "=", "=");

   package String_SHA1_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, GNAT.SHA1.Message_Digest, Hash, "=", "=");

   package String_File_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, GNATCOLL.VFS.Virtual_File,
      Hash, "=", GNATCOLL.VFS."=");

   protected Unit_Info is
      procedure Compute_Units_Info
        (Ctx : Helpers.App_Context; Out_Dir_File : GNATCOLL.VFS.Virtual_File);

      procedure Get_Out_File
        (X : String; F : out GNATCOLL.VFS.Virtual_File);
   private

      Unit_Out_Map : String_File_Maps.Map;
   end Unit_Info;

   protected body Unit_Info is
      procedure Compute_Units_Info
        (Ctx : Helpers.App_Context; Out_Dir_File : GNATCOLL.VFS.Virtual_File)
      is
         use GNATCOLL.Projects;
         use GNATCOLL.VFS;

         Project : Project_Tree_Access;

         procedure Process_File_No_Dir (Name : String) is
         begin
            Unit_Out_Map.Insert (To_Unbounded_String (Name), No_File);
         end Process_File_No_Dir;

         procedure Process_File_Absolute_Dir (Name : String) is
            File    : Virtual_File := Create (Filesystem_String (Name));
         begin
            Unit_Out_Map.Insert
              (To_Unbounded_String (Name),
               Out_Dir_File.Join (File.Base_Name));
         end Process_File_Absolute_Dir;

         procedure Process_File_Object_Dir (Name : String) is
            File    : Virtual_File := Create (Filesystem_String (Name));
            Info    : File_Info    := Project.Info (File);
            Actual  : Project_Type := Info.Project;
            Obj_Dir : Virtual_File := Actual.Object_Dir;
         begin
            Unit_Out_Map.Insert
              (To_Unbounded_String (Name),
               Obj_Dir.Join (Out_Dir_File).Join (File.Base_Name));
         end Process_File_Object_Dir;
      begin
         if Out_Dir_File = No_File then
            Session.Iterate_Files_To_Process
              (Process_File_No_Dir'Access);
         elsif Out_Dir_File.Is_Absolute_Path then
            Session.Iterate_Files_To_Process
              (Process_File_Absolute_Dir'Access);
         elsif Ctx.Provider.Kind in Helpers.Project_File then
            Project := Ctx.Provider.Project;
            Session.Iterate_Files_To_Process
              (Process_File_Object_Dir'Access);
         end if;
      end Compute_Units_Info;

      procedure Get_Out_File
        (X : String; F : out GNATCOLL.VFS.Virtual_File)
      is
         use String_File_Maps;

         C : Cursor := Unit_Out_Map.Find (To_Unbounded_String (X));
      begin
         if C = No_Element then
            raise Program_Error
               with "Cannot request Out_Dir of non-processed file " & X;
         end if;
         F := Element (C);
      end Get_Out_File;
   end Unit_Info;

   protected Incremental is
      procedure Compute_Change_Set;

      procedure Must_Reprocess
        (Unit   : LAL.Analysis_Unit;
         Result : out Boolean);

      function Get_SHA1 (File : String) return String;

   private

      function SHA1_Changed
        (File : String; SHA1 : GNAT.SHA1.Message_Digest)
         return Boolean;

      procedure Has_Dependency_Changed
        (Unit   : LAL.Analysis_Unit;
         Result : out Boolean);

      Change_Set   : String_Sets.Set;
      SHA1_Map     : String_SHA1_Maps.Map;
      To_Reprocess : String_Bool_Maps.Map;
   end Incremental;

   protected body Incremental is
      procedure Compute_Change_Set is
         use GNATCOLL;

         procedure Process_File (File : String) is
            File_Unbounded : Unbounded_String :=
               To_Unbounded_String (File);

            SHA1 : GNAT.SHA1.Message_Digest :=
               GNAT.SHA1.Digest (VFS.Create (VFS."+" (File)).Read_File.all);
         begin
            SHA1_Map.Insert (File_Unbounded, SHA1);
            if SHA1_Changed (File, SHA1) then
               Change_Set.Insert (File_Unbounded);
            end if;
         end Process_File;
      begin
         Session.Iterate_Files_To_Process (Process_File'Access);
      end Compute_Change_Set;

      function Get_SHA1 (File : String) return String is
         use String_SHA1_Maps;

         C : Cursor := SHA1_Map.Find (To_Unbounded_String (File));
      begin
         return (if C = No_Element then "" else Element (C));
      end Get_SHA1;

      function SHA1_Changed
        (File : String; SHA1 : GNAT.SHA1.Message_Digest)
         return Boolean
      is
         use GNATCOLL;

         SHA1_Comment_Length : constant Natural :=
            3 + GNAT.SHA1.Message_Digest'Length;
         -- ^ for "-- "

         Out_File : VFS.Virtual_File;

         Content_Access : GNAT.Strings.String_Access;
      begin
         Unit_Info.Get_Out_File (File, Out_File);

         if not Out_File.Is_Regular_File then
            return True;
         end if;

         Content_Access := Out_File.Read_File;

         if Content_Access.all'Length < SHA1_Comment_Length then
            return True;
         end if;

         return Content_Access (4 .. SHA1_Comment_Length) /= SHA1;
      end SHA1_Changed;

      procedure Has_Dependency_Changed
        (Unit   : LAL.Analysis_Unit;
         Result : out Boolean)
      is
         use type String_Bool_Maps.Cursor;

         Filename : Unbounded_String :=
            To_Unbounded_String (Unit.Get_Filename);

         Cursor   : String_Bool_Maps.Cursor :=
            To_Reprocess.Find (Filename);

         Inserted : Boolean;
      begin
         if Cursor /= String_Bool_Maps.No_Element then
            Result := String_Bool_Maps.Element (Cursor);
            return;
         end if;

         Result := Change_Set.Contains (Filename);
         To_Reprocess.Insert (Filename, Result, Cursor, Inserted);

         if Result then
            return;
         end if;

         for Dep of Utils.Imported_Units (Unit) loop
            Has_Dependency_Changed (Dep, Result);
            if Result then
               To_Reprocess.Replace_Element (Cursor, True);
               return;
            end if;
         end loop;

         Result := False;
      end Has_Dependency_Changed;

      procedure Must_Reprocess
        (Unit   : LAL.Analysis_Unit;
         Result : out Boolean)
      is
         procedure Check_Body (CU : LAL.Compilation_Unit'Class) is
         begin
            if CU.P_Unit_Kind in LALCO.Unit_Specification then
               declare
                  Body_Part : LAL.Body_Node := CU.P_Decl.P_Body_Part_For_Decl;
               begin
                  if not Body_Part.Is_Null then
                     Has_Dependency_Changed (Body_Part.Unit, Result);
                  end if;
               end;
            end if;
         end Check_Body;
      begin
         Has_Dependency_Changed (Unit, Result);

         if not Result then
            case Unit.Root.Kind is
               when LALCO.Ada_Compilation_Unit =>
                  Check_Body (Unit.Root.As_Compilation_Unit);
               when LALCO.Ada_Compilation_Unit_List =>
                  for CU of Unit.Root.As_Compilation_Unit_List loop
                     Check_Body (CU);
                     if Result then
                        return;
                     end if;
                  end loop;
               when others =>
                  null;
            end case;
         end if;
      end Must_Reprocess;
   end Incremental;

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

      Session.Set_Files_To_Process (Files);
      Unit_Info.Compute_Units_Info (Ctx, Out_Dir_File);
      Incremental.Compute_Change_Set;

      if Optimize.Get then
         Analysis.Summaries := new Analysis.Summaries_Map;
      end if;

      Put_Line ("Instrument");
   end Setup;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      use type LALCO.Ada_Node_Kind_Type;

      Must_Reprocess : Boolean;
   begin
      Incremental.Must_Reprocess (Unit, Must_Reprocess);
      if not Must_Reprocess then
         return;
      end if;

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

      Total_Written : Natural := 0;

      Post_Action_Count : Natural := Post_Actions.Actions.Length;
   begin
      if Post_Action_Count > 0 then
         Put_Line ("Apply" & Post_Action_Count'Image & " Global Changes");
      end if;

      Reparse_All_Units
        (First_Context,
         Jobs (Jobs'First + 1 .. Jobs'Last),
         All_Units);

      Post_Actions.Actions.Perform_Actions (First_Context, All_Units);

      Put_Line ("Output units");

      --  output units
      for Unit of All_Units loop
         declare
            Unit_File_Name  : String := Unit.Get_Filename;
            Was_Reprocessed : Boolean;
            Out_File        : GNATCOLL.VFS.Virtual_File;
            SHA1            : String :=
              (if No_Hash.Get
               then ""
               else Incremental.Get_SHA1 (Unit_File_Name));
         begin
            Incremental.Must_Reprocess (Unit, Was_Reprocessed);
            if Was_Reprocessed then
               Unit_Info.Get_Out_File (Unit_File_Name, Out_File);
               Output_Unit
                 (Unit     => Unit,
                  Out_File => Out_File,
                  SHA1     => SHA1);
               Total_Written := Total_Written + 1;
            end if;
         end;
      end loop;

      if Total_Written = 0 then
         Put_Line ("Done: everything up-to-date.");
      else
         Put_Line ("Done: instrumented" & Total_Written'Image & " units.");
      end if;
   end Post_Process;
begin
   App.Run;
end AGC;
