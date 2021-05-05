with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang;

with GNATCOLL.Projects; use GNATCOLL;

with Session;
with Utils;

function Pass.Check_Consistency
  (App_Ctx                : Helpers.App_Context;
   Ctx                    : Analysis.Analysis_Context;
   Processed_Units_Vector : Helpers.Unit_Vectors.Vector)
   return Boolean
is
   package LAL renames Libadalang.Analysis;

   package Analysis_Unit_Sets is new Ada.Containers.Hashed_Sets
     (LAL.Analysis_Unit, LAL.Hash, LAL."=", LAL."=");

   Processed_Units : Analysis_Unit_Sets.Set;
   Files_To_Check  : Helpers.String_Vectors.Vector;
   Result          : Boolean := True;

   procedure Populate_From_Default is null;

   procedure Populate_From_Project_File
     (Project : Projects.Project_Tree_Access)
   is
   begin
      Helpers.List_Sources_From_Project
        (Project.all, True, Files_To_Check);
   end Populate_From_Project_File;

   procedure Populate_From_Auto_Dir
     (Files : Helpers.String_Vectors.Vector)
   is
   begin
      Files_To_Check := Files;
   end Populate_From_Auto_Dir;

   function Check_File (F : String) return Boolean is
      Unit : LAL.Analysis_Unit := Ctx.Get_From_File (F);
   begin
      if not Processed_Units.Contains (Unit) then
         for Dep of Utils.Imported_Units (Unit, All_Visible => True) loop
            if Processed_Units.Contains (Dep) then
               Put_Line
                 ("error: " & Utils.Base_Name (F) & " must be "
                  & "instrumented as it depends on instrumented unit "
                  & Utils.Base_Name (LAL.Get_Filename (Dep)));
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Check_File;
begin
   case App_Ctx.Provider.Kind is
      when Helpers.Default =>
         Populate_From_Default;
      when Helpers.Project_File =>
         Populate_From_Project_File (App_Ctx.Provider.Project);
      when Helpers.Auto_Dir =>
         Populate_From_Auto_Dir (App_Ctx.Provider.Found_Files);
   end case;

   for Processed_Unit of Processed_Units_Vector loop
      Processed_Units.Insert (Processed_Unit);
   end loop;

   for File of Files_To_Check loop
      Result := Result and Check_File (To_String (File));
   end loop;

   return Result;
end Pass.Check_Consistency;
