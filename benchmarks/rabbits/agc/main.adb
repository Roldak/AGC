with AGC;
with AGC.Standard;
with AGC.Storage.Get;
with System;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Worlds;
with Entities.Rabbits;
with Entities.Grass;
with Entities.Wolves;
procedure Main is
   AGC_Base_Root_Count : constant Natural := AGC.Root_Count;
   pragma Default_Storage_Pool (AGC.Storage.Get.Pool);
   W           : aliased Worlds.World;
   AGC_Dummy_0 : constant AGC.Empty_Type :=
     AGC.Push_Root (W'Address, Worlds.AGC_Visit_World_Private'Address);
   Stage : Natural := 0;
   procedure Print_Stats is
      Rabbits, Wolves, Grass : Natural;
   begin
      W.Stats (Rabbits, Wolves, Grass);
      Put_Line ("Stage" & Stage'Image & " :");
      Put_Line ("  Rabbits :" & Rabbits'Image);
      Put_Line ("  Wolves  :" & Wolves'Image);
      Put_Line ("  Grass   :" & Grass'Image);
   end Print_Stats;
begin
   for I in 1 .. 80 loop
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Rabbits.Create;
         AGC_Dummy_1    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Rabbits.Create;
         AGC_Dummy_2    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Rabbits.Create;
         AGC_Dummy_3    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Rabbits.Create;
         AGC_Dummy_4    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Rabbits.Create;
         AGC_Dummy_5    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Grass.Create;
         AGC_Dummy_6    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Grass.Create;
         AGC_Dummy_7    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Grass.Create;
         AGC_Dummy_8    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
      declare
         AGC_Root_Count : constant Natural             := AGC.Root_Count;
         AGC_Temp_0 : aliased Worlds.Entity_Access := Entities.Grass.Create;
         AGC_Dummy_9    : constant AGC.Empty_Type      :=
           AGC.Push_Root
             (AGC_Temp_0'Address, Worlds.AGC_Visit_Entity_Access'Address);
      begin
         W.Spawn (AGC_Temp_0);
         AGC.Pop_Roots (AGC_Root_Count);
      end;
   end loop;
   for I in 1 .. 8 loop
      declare
         X, Y : Natural;
      begin
         Worlds.Grid.Random_Position (X, Y);
         for J in 1 .. 10 loop
            declare
               AGC_Root_Count : constant Natural             := AGC.Root_Count;
               AGC_Temp_0     : aliased Worlds.Entity_Access :=
                 Entities.Wolves.Create (X, Y);
               AGC_Dummy_10 : constant AGC.Empty_Type :=
                 AGC.Push_Root
                   (AGC_Temp_0'Address,
                    Worlds.AGC_Visit_Entity_Access'Address);
            begin
               W.Spawn (AGC_Temp_0);
               AGC.Pop_Roots (AGC_Root_Count);
            end;
         end loop;
      end;
   end loop;
   while W.Is_Running and Stage < 2_000 loop
      Stage := Stage + 1;
      W.Update;
   end loop;
   Print_Stats;
   AGC.Pop_Roots (AGC_Base_Root_Count);
end Main;
