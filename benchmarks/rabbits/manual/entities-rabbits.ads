with Entities.Positioned;

package Entities.Rabbits is
   type Rabbit is new Positioned.Positioned with private;

   function Create return Entity_Access;
   function Create (X, Y : Natural) return Entity_Access;

   overriding procedure Start
     (R : in out Rabbit; W : in out World);
   overriding procedure Update
     (R : in out Rabbit; W : in out World);
private
   type Rabbit is new Positioned.Positioned with record
      Age  : Natural;
      Food : Natural;
   end record;
end Entities.Rabbits;
