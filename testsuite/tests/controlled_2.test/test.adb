with AGC;
with Ada.Containers.Hashed_Maps;

procedure Test is
   function Hash (X : Integer) return Ada.Containers.Hash_Type is
     (0);

   package Maps is new Ada.Containers.Hashed_Maps
     (Integer, Integer, Hash, "=", "=");

   type T is record
      X : Maps.Map;
      V : Integer;
   end record;

   type T_Access is access T;

   procedure Main is
      X : T_Access := new T'(V => 42, others => <>);
   begin
      null;
   end Main;
begin
   Main;
   AGC.Collect;
end Test;

