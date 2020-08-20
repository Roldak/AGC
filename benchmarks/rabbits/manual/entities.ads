with Worlds; use Worlds;

package Entities is
   type Entity is abstract tagged private;

   procedure Start (E : in out Entity; W : in out World) is abstract;
   procedure Update (E : in out Entity; W : in out World) is abstract;

   procedure Delete (E : in out Entity);

   function Is_Alive (E : in Entity) return Boolean;
private
   type Entity is abstract tagged record
      Alive : Boolean := True;
   end record;

   function Is_Alive (E : in Entity) return Boolean is (E.Alive);
end Entities;
