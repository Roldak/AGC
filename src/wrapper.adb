with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNATCOLL.VFS; use GNATCOLL.VFS;

function Wrapper return Boolean is
   Instr_Dir         : constant String := "agc-instr";
   Output_Dir_Opt    : constant String := "--output-dir=" & Instr_Dir;
   Src_Subdirs_Opt   : constant String := "--src-subdirs=" & Instr_Dir;
   Implicit_With_Opt : constant String := "--implicit-with=agc_runtime";

   function Extract_GPRBuild_Arguments
     (From_Index : Positive) return Argument_List
   is
   begin
      for I in From_Index .. Argument_Count loop
         if Argument (I) = "-P" then
            return
              (1 => new String'("-P"),
               2 => new String'(Argument (I + 1)))
               & Extract_GPRBuild_Arguments (I + 2);
         elsif Argument (I)(1) /= '-' then
            return
               (1 => new String'(Argument (I)))
               & Extract_GPRBuild_Arguments (I + 1);
         end if;
      end loop;
      return (1 .. 0 => <>);
   end Extract_GPRBuild_Arguments;

   procedure Run_AGC
     (Last_AGC_Arg_Index       : Natural;
      First_GPRBuild_Arg_Index : Positive)
   is
      AGC_Success        : Boolean;
      AGC_Base_Args      : Argument_List (1 .. Last_AGC_Arg_Index + 1);
      AGC_Extracted_Args : Argument_List :=
         Extract_GPRBuild_Arguments (First_GPRBuild_Arg_Index);
      AGC_Command        : Virtual_File := Locate_On_Path ("agc");
   begin
      for I in 1 .. Last_AGC_Arg_Index loop
         AGC_Base_Args (I) := new String'(Argument (I));
      end loop;
      AGC_Base_Args (Last_AGC_Arg_Index + 1) := new String'(Output_Dir_Opt);

      Spawn
        (AGC_Command.Display_Full_Name,
         AGC_Base_Args & AGC_Extracted_Args,
         AGC_Success);

      if not AGC_Success then
         OS_Exit (1);
      end if;
   end Run_AGC;

   procedure Run_GPRBuild (I : Positive) is
      Last_Arg_Index : constant Positive := Argument_Count - I + 1;

      Build_Success  : Boolean;
      Build_Args     : Argument_List (1 .. Last_Arg_Index + 2);
      Build_Command  : Virtual_File := Locate_On_Path ("gprbuild");
   begin
      for J in 1 .. Argument_Count - I + 1 loop
         Build_Args (J) := new String'(Argument (I + J - 1));
      end loop;
      Build_Args (Last_Arg_Index + 1) := new String'(Src_Subdirs_Opt);
      Build_Args (Last_Arg_Index + 2) := new String'(Implicit_With_Opt);

      Spawn
         (Build_Command.Display_Full_Name,
          Build_Args,
          Build_Success);

      if not Build_Success then
         OS_Exit (2);
      end if;
   end Run_GPRBuild;
begin
   for I in 1 .. Argument_Count loop
      if Argument (I) = "--" and then I + 1 <= Argument_Count then
         if Argument (I + 1) = "gprbuild" and I + 2 <= Argument_Count then
            Run_AGC (I - 1, I + 2);
            Run_GPRBuild (I + 2);
            return True;
         end if;
      end if;
   end loop;
   return False;
end Wrapper;
