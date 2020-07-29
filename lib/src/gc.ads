with Ada.Containers;

with System;

package GC is
   function Root_Count return Natural
      with Inline;

   procedure Push_Root (X, Visitor : System.Address)
      with Inline;

   procedure Pop_Roots (X : Natural)
      with Inline;

   generic
      type T (<>) is private;
   function Register (X : access T) return access T
      with Inline;

   generic
      type T (<>) is private;
   procedure No_Op (X : System.Address)
      with Inline;

   generic
      type T (<>) is private;
      type T_Access is access all T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Access_Type (X : System.Address)
      with Inline;

   generic
      type T is private;
      type I is (<>);
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Array_Type (X : System.Address)
      with Inline;

   procedure Collect;

   procedure Print_Stats;
end GC;
