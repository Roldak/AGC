with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

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

procedure Pass.Output_Unit
  (Unit      : Libadalang.Analysis.Analysis_Unit;
   With_SHA1 : Boolean)
is
   package LAL     renames Libadalang.Analysis;
   package LALU    renames Libadalang.Unparsing;

   use type Strings.XString;
   use type VFS.Virtual_File;

   Filename : String := Unit.Get_Filename;
   Content  : String := LALU.Unparse (Unit.Root, Preserve_Formatting => True);

   Out_File : VFS.Virtual_File := Session.Get_Out_File (Filename);
begin
   if Out_File = VFS.No_File then
      Put_Line ("--  " & Utils.Base_Name (Filename));
      Put_Line (Content);
      New_Line;
   else
      VFS.Make_Dir (Out_File.Dir, Recursive => True);
      declare
         New_Unit : VFS.Writable_File := Out_File.Write_File;

         SHA1_Comment : String :=
           (if With_Sha1
            then "-- " & Session.Get_SHA1 (Filename) & LF
            else "");
      begin
         VFS.Write (New_Unit, SHA1_Comment & Content);
         VFS.Close (New_Unit);
      end;
   end if;
end Pass.Output_Unit;
