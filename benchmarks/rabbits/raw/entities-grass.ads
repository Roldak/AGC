with Entities.Positioned;

package Entities.Grass is
   type Grass is new Positioned.Positioned with private;

   function Create return Entity_Access;
   function Create (X, Y : Natural) return Entity_Access;

   overriding procedure Start
     (G : in out Grass; W : in out World);
   overriding procedure Update
     (G : in out Grass; W : in out World);

   function Eat (G : in out Grass) return Boolean;
private
   type Grass is new Positioned.Positioned with record
      Content : Natural;
   end record;
end Entities.Grass;
