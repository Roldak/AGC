with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Helpers;

procedure Main is
   package Helpers renames Libadalang.Helpers;
   package LAL renames Libadalang.Analysis;
   package Slocs renames Langkit_Support.Slocs;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   package App is new Helpers.App
     (Name         => "AGC",
      Description  => "Garbage collection for Ada",
      Process_Unit => Process_Unit);

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
   begin
      null;
   end Process_Unit;
begin
   App.Run;
end Main;
