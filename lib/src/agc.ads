with Ada.Containers;

with System;

package AGC is
   function Root_Count return Natural
      with Inline;

   procedure Push_Root (X, Visitor : System.Address)
      with Inline;

   procedure Pop_Roots (X : Natural)
      with Inline;

   procedure Register (X : System.Address)
      with Inline;

   procedure No_Op (X : System.Address) is null
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
      type T_Array is array (I) of T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Constrained_Array_Type (X : System.Address)
      with Inline;

   generic
      type T is private;
      type I is (<>);
      type T_Array is array (I range <>) of T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Unconstrained_Array_Type (X : System.Address)
      with Inline;

   procedure Collect;

   procedure Print_Stats;
end AGC;
