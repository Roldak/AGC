with Ada.Containers;

with System;

package GC is
   procedure Push_Reachable (X : System.Address);

   procedure Pop_Reachable (Count : Ada.Containers.Count_Type);

   function Register
     (Site_Id : Natural; X : access Integer) return access Integer;

   procedure Untemp (Site_Id : Natural);

   procedure Collect;
end GC;
