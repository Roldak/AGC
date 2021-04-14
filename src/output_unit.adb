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
  (Unit     : Libadalang.Analysis.Analysis_Unit;
   Out_File : VFS.Virtual_File;
   SHA1     : String)
is
   package LAL     renames Libadalang.Analysis;
   package LALU    renames Libadalang.Unparsing;

   use type Strings.XString;
   use type VFS.Virtual_File;

   Content  : String := LALU.Unparse (Unit.Root, Preserve_Formatting => True);
begin
   if Out_File = VFS.No_File then
      Put_Line ("--  " & Utils.Base_Name (Unit.Get_Filename));
      Put_Line (Content);
      New_Line;
   else
      VFS.Make_Dir (Out_File.Dir, Recursive => True);
      declare
         New_Unit : VFS.Writable_File := Out_File.Write_File;

         SHA1_Comment : String :=
           (if SHA1 = ""
            then ""
            else "-- " & SHA1 & Ada.Characters.Latin_1.LF);
      begin
         VFS.Write (New_Unit, SHA1_Comment & Content);
         VFS.Close (New_Unit);
      end;
   end if;
end Output_Unit;
