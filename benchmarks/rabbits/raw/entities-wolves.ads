with Entities.Positioned;

package Entities.Wolves is
   type Wolf is new Positioned.Positioned with private;

   function Create return Entity_Access;
   function Create (X, Y : Natural) return Entity_Access;

   overriding procedure Start
     (R : in out Wolf; W : in out World);
   overriding procedure Update
     (R : in out Wolf; W : in out World);
private
   type Wolf is new Positioned.Positioned with record
      Age  : Natural;
      Food : Natural;
   end record;
end Entities.Wolves;
