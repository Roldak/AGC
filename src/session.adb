with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Strings;
with GNATCOLL.VFS;

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
         begin
            Files_To_Process.Insert (Full_Path_Unbounded);
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

end Session;
