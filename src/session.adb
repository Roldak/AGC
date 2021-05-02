with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects;
with GNATCOLL.Strings;
with GNATCOLL.VFS;

with GNAT.Strings;

with Langkit_Support.Text;
with Libadalang.Common;
with Libadalang.Unit_Files;

with Analysis;
with Utils;

package body Session is
   package LALCO renames Libadalang.Common;

   procedure Init_Session
     (Ctx          : App_Context;
      Out_Dir_File : GNATCOLL.VFS.Virtual_File;
      File_Names   : String_Vectors.Vector;
      Optim_Level  : Optimization_Level_Type)
   is
      use GNATCOLL;
   begin
      for File_Name of File_Names loop
         declare
            File : VFS.Virtual_File :=
               VFS.Create_From_Base (VFS."+" (To_String (File_Name)));

            Full_Path : String := String (VFS.Full_Name (File).all);

            Full_Path_Unbounded : Unbounded_String :=
               To_Unbounded_String (Full_Path);
         begin
            Files_To_Process.Insert (Full_Path_Unbounded);
         end;
      end loop;

      Unit_Info.Compute_Units_Info (Ctx, Out_Dir_File);
      Incremental.Compute_Change_Set;

      Optimization_Level := Optim_Level;
      if Optim_Level /= None then
         Analysis.Summaries := new Analysis.Summaries_Map;
      end if;
   end Init_Session;

   function Is_File_To_Process (File_Name : String) return Boolean is
   begin
      return Files_To_Process.Contains
        (To_Unbounded_String (File_Name));
   end Is_File_To_Process;

   procedure Iterate_Files_To_Process
     (Process: access procedure (File : String))
   is
   begin
      for File of Files_To_Process loop
         Process (To_String (File));
      end loop;
   end Iterate_Files_To_Process;

   procedure Include_New_Unit
     (Derived_From : LAL.Analysis_Unit; Name : String)
   is
      use GNATCOLL.VFS;

      Derived_Unit_Name : String := Derived_From.Get_Filename;
      Derived_Out_File  : Virtual_File := Get_Out_File (Derived_Unit_Name);

      Base_Name         : String := Utils.Base_Name (Name);
      Hypothetical_Path : Virtual_File :=
         Create (+Derived_Unit_Name).Dir.Join (+Base_Name);
      Hypothetical_Full : String := Hypothetical_Path.Display_Full_Name;
   begin
      Unit_Info.Add_Unit_Info
        (Hypothetical_Full,
         Derived_Out_File.Dir.Join (+Base_Name));

      Incremental.Requires_Processing (Hypothetical_Full);
   end Include_New_Unit;

   function Get_Out_File (X : String) return GNATCOLL.VFS.Virtual_File is
      Result : GNATCOLL.VFS.Virtual_File;
   begin
      Unit_Info.Get_Out_File (X, Result);
      return Result;
   end Get_Out_File;

   function Get_SHA1 (File : String) return String is
     (Incremental.Get_SHA1 (File));

   function Must_Reprocess (Unit : LAL.Analysis_unit) return Boolean is
      Result : Boolean;
   begin
      Incremental.Must_Reprocess (unit, Result);
      return Result;
   end Must_Reprocess;

   function Get_Optimization_Level return Optimization_Level_Type is
     (Optimization_Level);

   protected body Unit_Info is
      procedure Compute_Units_Info
        (Ctx : App_Context; Out_Dir_File : GNATCOLL.VFS.Virtual_File)
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
         elsif Ctx.Provider.Kind in Project_File then
            Project := Ctx.Provider.Project;
            Session.Iterate_Files_To_Process
              (Process_File_Object_Dir'Access);
         else
            Session.Iterate_Files_To_Process
              (Process_File_Absolute_Dir'Access);
         end if;
      end Compute_Units_Info;

      procedure Add_Unit_Info
        (Name : String; Out_File : GNATCOLL.VFS.Virtual_File)
      is
      begin
         Unit_Out_Map.Insert (To_Unbounded_String (Name), Out_File);
      end Add_Unit_Info;

      procedure Get_Out_File
        (Name : String; F : out GNATCOLL.VFS.Virtual_File)
      is
         use String_File_Maps;

         C : Cursor := Unit_Out_Map.Find (To_Unbounded_String (Name));
      begin
         if C = No_Element then
            raise Program_Error
               with "Cannot request Out_Dir of non-processed file " & Name;
         end if;
         F := Element (C);
      end Get_Out_File;
   end Unit_Info;

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
               To_Reprocess.Insert (File_Unbounded, True);
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

      procedure Requires_Processing (File : String) is
      begin
         To_Reprocess.Include (To_Unbounded_String (File), True);
      end Requires_Processing;

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

         To_Reprocess.Insert (Filename, False, Cursor, Inserted);

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
         use Langkit_Support.Text;

         Provider : LAL.Unit_Provider_Reference := Unit.Context.Unit_Provider;

         procedure Check_Body (CU : LAL.Compilation_Unit'Class) is
         begin
            if CU.P_Unit_Kind in LALCO.Unit_Specification then
               declare
                  FQN : LAL.Unbounded_Text_Type_Array :=
                     CU.P_Syntactic_Fully_Qualified_Name;
                  Body_Unit : LAL.Analysis_Unit'Class := Provider.Get.Get_Unit
                    (Unit.Context,
                     Utils.Dot_Concat (FQN),
                     LALCO.Unit_Body);
               begin
                  if not Body_Unit.Root.Is_Null then
                     Has_Dependency_Changed
                       (LAL.Analysis_Unit (Body_Unit), Result);
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
end Session;
