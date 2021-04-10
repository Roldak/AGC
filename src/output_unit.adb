with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

with GNATCOLL; use GNATCOLL;
with GNATCOLL.Strings;
with GNATCOLL.VFS;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Helpers;
with Libadalang.Unparsing;

with Session;
with Utils;

procedure Output_Unit
  (Unit    : Libadalang.Analysis.Analysis_Unit;
   Out_Dir : VFS.Virtual_File)
is
   package LAL     renames Libadalang.Analysis;
   package LALU    renames Libadalang.Unparsing;

   use type Strings.XString;
   use type VFS.Virtual_File;

   Content  : String := LALU.Unparse (Unit.Root);
   Basename : String := Utils.Base_Name (Unit.Get_Filename);
begin
   if Out_Dir = VFS.No_File then
      Put_Line ("--  " & Basename);
      Put_Line (Content);
      New_Line;
   else
      declare
         New_Unit : VFS.Writable_File :=
            VFS.Join (Out_Dir, VFS."+" (Basename)).Write_File;

         SHA1 : String := Session.Get_Processed_File_SHA1 (Unit.Get_Filename);

         SHA1_Comment : String := "-- " & SHA1 & Ada.Characters.Latin_1.LF;
      begin
         VFS.Write (New_Unit, SHA1_Comment & Content);
         VFS.Close (New_Unit);
      end;
   end if;
end Output_Unit;
