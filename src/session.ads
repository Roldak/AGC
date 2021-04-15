with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.VFS;
with GNAT.SHA1;

with Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Post_Actions;

package Session is
   package LAL renames Libadalang.Analysis;

   procedure Init_Session
     (Ctx          : App_Context;
      Out_Dir_File : GNATCOLL.VFS.Virtual_File;
      File_Names   : String_Vectors.Vector);

   function Is_File_To_Process (File_Name : String) return Boolean;

   procedure Iterate_Files_To_Process
     (Process: access procedure (File : String));

   procedure Include_New_Unit
     (Derived_From : LAL.Analysis_Unit; Name : String);

   function Get_Out_File (X : String) return GNATCOLL.VFS.Virtual_File;

   function Get_SHA1 (File : String) return String;

   function Must_Reprocess (Unit : LAL.Analysis_unit) return Boolean;

private

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Unbounded_String, Hash, "=", "=");

   package String_Bool_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Boolean, Hash, "=", "=");

   package String_SHA1_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, GNAT.SHA1.Message_Digest, Hash, "=", "=");

   package String_File_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, GNATCOLL.VFS.Virtual_File,
      Hash, "=", GNATCOLL.VFS."=");

   Files_To_Process : String_Sets.Set;

   protected Unit_Info is
      procedure Compute_Units_Info
        (Ctx : App_Context; Out_Dir_File : GNATCOLL.VFS.Virtual_File);

      procedure Add_Unit_Info
        (Name : String; Out_File : GNATCOLL.VFS.Virtual_File);

      procedure Get_Out_File
        (Name : String; F : out GNATCOLL.VFS.Virtual_File);
   private

      Unit_Out_Map : String_File_Maps.Map;
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

      SHA1_Map     : String_SHA1_Maps.Map;
      To_Reprocess : String_Bool_Maps.Map;
   end Incremental;
end Session;
