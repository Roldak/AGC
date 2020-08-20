with Ada.Text_IO; use Ada.Text_IO;

with Worlds;
with Entities.Rabbits;
with Entities.Grass;
with Entities.Wolves;

procedure Main is
   W : Worlds.World;

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
      W.Spawn (Entities.Rabbits.Create);
      W.Spawn (Entities.Rabbits.Create);
      W.Spawn (Entities.Rabbits.Create);
      W.Spawn (Entities.Rabbits.Create);
      W.Spawn (Entities.Rabbits.Create);
      W.Spawn (Entities.Grass.Create);
      W.Spawn (Entities.Grass.Create);
      W.Spawn (Entities.Grass.Create);
      W.Spawn (Entities.Grass.Create);
   end loop;

   for I in 1 .. 8 loop
      declare
         X, Y : Natural;
      begin
         Worlds.Grid.Random_Position (X, Y);
         for J in 1 .. 10 loop
            W.Spawn (Entities.Wolves.Create (X, Y));
         end loop;
      end;
   end loop;

   while W.Is_Running and Stage < 2000 loop
      Stage := Stage + 1;
      W.Update;
   end loop;

   Print_Stats;
end Main;
