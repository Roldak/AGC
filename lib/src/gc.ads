with Ada.Containers;

with System;

package GC is
   function Root_Count return Natural;

   procedure Push_Root (X, Visitor : System.Address);

   procedure Pop_Roots (X : Natural);

   generic
      type T (<>) is private;
   function Register (X : access T) return access T;

   generic
      type T (<>) is private;
   procedure No_Op (X : System.Address);

   generic
      type T (<>) is private;
      type T_Access is access all T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Access_Type (X : System.Address);

   generic
      type T is private;
      type I is (<>);
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Array_Type (X : System.Address);

   procedure Collect;

   procedure Print_Stats;
end GC;
