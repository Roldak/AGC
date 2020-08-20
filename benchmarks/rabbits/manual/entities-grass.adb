package body Entities.Grass is
   function Create return Entity_Access is
      X, Y : Natural;
   begin
      Worlds.Grid.Random_Position (X, Y);
      return Create (X, Y);
   end Create;

   function Create (X, Y : Natural) return Entity_Access is
   begin
      return G : Entity_Access := new Grass do
         Positioned_Access (G).Initialize (X, Y);
      end return;
   end Create;

   overriding procedure Start
     (G : in out Grass; W : in out World)
   is
   begin
      G.Content := 5;
   end Start;

   overriding procedure Update
     (G : in out Grass; W : in out World)
   is
      X : Natural := G.X;
      Y : Natural := G.Y;
   begin
      if Worlds.Grid.Moved (X, Y, Worlds.Grid.Random_Direction) then
         if not W.Has_Grass (X, Y) then
            W.Spawn (Create (X, Y));
         end if;
      end if;
   end Update;

   function Eat (G : in out Grass) return Boolean is
   begin
      if not G.Is_Alive then
         return False;
      end if;

      G.Content := G.Content - 1;
      if G.Content = 0 then
         G.Delete;
      end if;

      return True;
   end Eat;
end Entities.Grass;
