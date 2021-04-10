with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Strings;
with GNATCOLL.VFS;
with GNAT.Strings;

with Utils;

package body Session is
   procedure Set_Files_To_Process (File_Names : String_Vectors.Vector) is
      use GNATCOLL;
   begin
      for File_Name of File_Names loop
         declare
            File : VFS.Virtual_File :=
               VFS.Create_From_Base (VFS."+" (To_String (File_Name)));

            Full_Path : String := String (VFS.Full_Name (File).all);

            Full_Path_Unbounded : Unbounded_String :=
               To_Unbounded_String (Full_Path);

            SHA1 : GNAT.SHA1.Message_Digest :=
               GNAT.SHA1.Digest (File.Read_File.all);
         begin
            Files_To_Process.Insert (Full_Path_Unbounded);
            Files_SHA1.Insert (Full_Path_Unbounded, SHA1);
         end;
      end loop;
   end Set_Files_To_Process;

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

   function Get_Processed_File_SHA1 (File_Name : String) return String is
      use File_SHA1_Maps;

      C : Cursor := Files_SHA1.Find (To_Unbounded_String (File_Name));
   begin
      if C = No_Element then
         return "";
      else
         return Element (C);
      end if;
   end Get_Processed_File_SHA1;

   SHA1_Comment_Length : constant Natural :=
      3 + GNAT.SHA1.Message_Digest'Length;
   -- ^ for "-- "

   function Has_Changed
     (File_Name : String;
      Out_Dir   : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      use GNATCOLL;

      Out_File : VFS.Virtual_File :=
         VFS.Join (Out_Dir, VFS."+" (Utils.Base_Name (File_Name)));

      Content_Access       : GNAT.Strings.String_Access;
      Previous_SHA1_Digest : GNAT.SHA1.Message_Digest;
      Current_SHA1_Digest  : GNAT.SHA1.Message_Digest;
   begin
      if not Out_File.Is_Regular_File then
         return True;
      end if;

      Content_Access := Out_File.Read_File;

      if Content_Access.all'Length < SHA1_Comment_Length then
         return True;
      end if;

      Previous_SHA1_Digest :=
         Content_Access (4 .. GNAT.SHA1.Message_Digest'Length + 3);

      Current_SHA1_Digest :=
         Get_Processed_File_SHA1 (File_Name);

      return Previous_SHA1_Digest /= Current_SHA1_Digest;
   end Has_Changed;
end Session;
