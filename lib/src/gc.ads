with Ada.Containers;

with System;

package GC is
   function Root_Count return Natural;

   procedure Push_Root (X : System.Address);

   procedure Pop_Roots (X : Natural);

   function Register (X : access Integer) return access Integer;

   function Temp
     (Site_Id : Natural; X : access Integer) return access Integer;

   procedure Untemp (Site_Id : Natural);

   procedure Collect;

   procedure Print_Stats;
end GC;
