with Ada.Containers;

with System;

package GC is
   function Root_Count return Natural;

   procedure Push_Root (X : System.Address);

   procedure Pop_Roots (X : Natural);

   generic
      type T is private;
   function Register (X : access T) return access T;

   procedure Collect;

   procedure Print_Stats;
end GC;
