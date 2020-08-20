package Entities.Positioned is
   type Positioned is abstract new Entity with private;

   procedure Initialize
     (E : in out Positioned; X, Y : Natural);

   function X (P : Positioned) return Natural;
   function Y (P : Positioned) return Natural;

   procedure Relocate
     (P : aliased in out Positioned; W : in out World;
      New_X, New_Y : Natural);
private

   type Positioned is abstract new Entity with record
      P_X, P_Y : Natural;
   end record;
end Entities.Positioned;
