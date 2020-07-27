with Ada.Containers;

with System;

package GC is
   function Root_Count return Natural;

   procedure Push_Root (X, Visitor : System.Address);

   procedure Pop_Roots (X : Natural);

   generic
      type T is private;
   function Register (X : access T) return access T;

   generic
      type T is private;
   procedure No_Op (X : T);

   generic
      type T is private;
      type T_Access is access all T;
      with procedure Visit_Element (X : T);
   procedure Visit_Access_Type (X : T_Access);

   procedure Collect;

   procedure Print_Stats;
end GC;
