with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.VFS;
with GNAT.Sha1;

with Libadalang.Helpers; use Libadalang.Helpers;

with Post_Actions;

package Session is
   procedure Set_Files_To_Process (File_Names : String_Vectors.Vector);

   function Is_File_To_Process (File_Name : String) return Boolean;

   procedure Iterate_Files_To_Process
     (Process: access procedure (File : String));

   function Get_Processed_File_SHA1 (File_Name : String) return String;

   function Has_Changed
     (File_Name : String;
      Out_Dir   : GNATCOLL.VFS.Virtual_File) return Boolean;

   To_Do : Post_Actions.Actions;
private
   package String_Sets is new Ada.Containers.Hashed_Sets
     (Unbounded_String, Hash, "=", "=");

   package File_SHA1_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, GNAT.Sha1.Message_Digest, Hash, "=", "=");

   Files_To_Process : String_Sets.Set;
   Files_To_Reprocess : String_Sets.Set;

   Files_SHA1 : File_SHA1_Maps.Map;
end Session;
