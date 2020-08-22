with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Entities;
with Entities.Positioned;
with Entities.Rabbits;
with Entities.Grass;
with Entities.Wolves;

package body Worlds is
   use type Ada.Containers.Count_Type;

   procedure Free_Entity is new Ada.Unchecked_Deallocation
     (Entities.Entity'Class, Entity_Access);

   procedure Spawn (W : in out World; E : Entity_Access) is
   begin
      W.New_Entities.Append (E);
   end Spawn;

   procedure Update (W : in out World) is
   begin
      for I in reverse W.Entities.First_Index .. W.Entities.Last_Index loop
         declare
            E : Entity_Access := W.Entities (I);
         begin
            E.all.Update (W);
            if not E.all.Is_Alive then
               W.Entities.Delete (I);

               if E.all in Entities.Positioned.Positioned'Class then
                  declare
                     P : constant Positioned_Access := Positioned_Access (E);
                  begin
                     W.Cells.Del (P.X, P.Y, P);
                  end;
               end if;

               Free_Entity (E);
            end if;
         end;
      end loop;

      for E of W.New_Entities loop
         E.all.Start (W);
         W.Entities.Append (E);

         if E.all in Entities.Positioned.Positioned'Class then
            declare
               P : constant Positioned_Access := Positioned_Access (E);
            begin
               W.Cells.Put (P.X, P.Y, P);
            end;
         end if;
      end loop;

      W.New_Entities.Clear;
   end Update;

   procedure Move
     (W : in out World; I : Positioned_Access;
      Old_X, Old_Y : Natural)
   is
   begin
      W.Cells.Del (Old_X, Old_Y, I);
      W.Cells.Put (I.X, I.Y, I);
   end Move;

   function Is_Running (W : in out World) return Boolean is
   begin
      return not W.Entities.Is_Empty or else not W.New_Entities.Is_Empty;
   end Is_Running;

   function Located
     (W : in World; X, Y : Natural) return Positioned_Array
   is
      Acc : constant Grid.Item_Vectors.Vector := W.Cells.Get (X, Y);
   begin
      return Res : Positioned_Array (Acc.First_Index .. Acc.Last_Index) do
         for I in Res'Range loop
            Res (I) := Acc (I);
         end loop;
      end return;
   end Located;

   procedure Stats
     (W : World; Rabbits, Wolves, Grass : out Natural)
   is
      R, F, G : Natural := 0;
   begin
      for E of W.Entities loop
         if E.all in Entities.Rabbits.Rabbit'Class then
            R := R + 1;
         elsif E.all in Entities.Wolves.Wolf'Class then
            F := F + 1;
         elsif E.all in Entities.Grass.Grass'Class then
            G := G + 1;
         end if;
      end loop;

      Rabbits := R;
      Wolves := F;
      Grass := G;
   end Stats;

   function Has_Grass (W : World; X, Y : Natural) return Boolean is
   begin
      for E of W.Cells.Get (X, Y) loop
         if E.all in Entities.Grass.Grass'Class then
            return True;
         end if;
      end loop;
      return False;
   end Has_Grass;
end Worlds;
