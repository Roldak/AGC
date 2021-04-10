with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.VFS;

with Libadalang.Helpers; use Libadalang.Helpers;

with Post_Actions;

package Session is
   procedure Set_Files_To_Process (File_Names : String_Vectors.Vector);

   function Is_File_To_Process (File_Name : String) return Boolean;

   procedure Iterate_Files_To_Process
     (Process: access procedure (File : String));

private

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Unbounded_String, Hash, "=", "=");

   Files_To_Process : String_Sets.Set;
end Session;
