with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL; use GNATCOLL;
with GNATCOLL.Strings;
with GNATCOLL.VFS;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Helpers;
with Libadalang.Unparsing;

procedure Output_Unit
  (Job_Ctx      : Libadalang.Helpers.App_Job_Context;
   Unit         : Libadalang.Analysis.Analysis_Unit;
   Out_Dir_Path : Strings.XString)
is
   package LAL     renames Libadalang.Analysis;
   package LALU    renames Libadalang.Unparsing;

   use type Strings.XString;

   Content : String := LALU.Unparse (Unit.Root);

   Orig_Unit : VFS.Virtual_File :=
      VFS.Create (VFS."+" (Unit.Get_Filename));

   Unit_Base_Name : VFS.Filesystem_String :=
      VFS.Base_Name (Orig_Unit);
begin
   if Out_Dir_Path = Strings.Null_XString then
      Put_Line ("--  " & VFS."+" (Unit_Base_Name));
      Put_Line (Content);
      New_Line;
   else
      declare
         Out_Dir : VFS.Virtual_File :=
            VFS.Create_From_Base (VFS."+" (Strings.To_String (Out_Dir_Path)));

         New_Unit : VFS.Writable_File :=
            VFS.Join (Out_Dir, Unit_Base_Name).Write_File;
      begin
         VFS.Write (New_Unit, Content);
         VFS.Close (New_Unit);
      end;
   end if;
end Output_Unit;
