with Ada.Containers;

with System;
with System.Storage_Elements;

package AGC is
   type Empty_Type is null record;

   type Non_Managed is access all Empty_Type;

   type Finalizer is access procedure (X : System.Address);

   function Root_Count return Natural
      with Inline;

   function Push_Root
     (X, Visitor : System.Address) return Empty_Type
      with Inline;

   procedure Pop_Roots (X : Natural)
      with Inline;

   generic
      type T (<>) is limited private;
      type Named_Access is access T;
      Is_Generalized_Access : Boolean;
      with procedure Visit_Element (X : System.Address);
   package Access_Type_Operations is

      function Register
        (Acc : Named_Access) return Named_Access
         with Inline;

      procedure Visit_Access_Type (X : System.Address)
         with Inline;

      procedure Mark_And_Visit_Access_Type (X : System.Address)
         with Inline;

   private

      procedure Finalize_From_Object_Address
        (Object_Address : System.Address)
         with Inline;

   end Access_Type_Operations;

   procedure No_Op (X : System.Address) is null
      with Inline;

   generic
      type T is private;
      type I is (<>);
      type T_Array is array (I) of T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Constrained_Array_1_Type (X : System.Address)
      with Inline;

   generic
      type T is private;
      type I is (<>);
      type T_Array is array (I range <>) of T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Unconstrained_Array_1_Type (X : System.Address)
      with Inline;

   generic
      type T is private;
      type I_1 is (<>);
      type I_2 is (<>);
      type T_Array is array (I_1, I_2) of T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Constrained_Array_2_Type (X : System.Address)
      with Inline;

   generic
      type T is private;
      type I_1 is (<>);
      type I_2 is (<>);
      type T_Array is array (I_1 range <>, I_2 range <>) of T;
      with procedure Visit_Element (X : System.Address);
   procedure Visit_Unconstrained_Array_2_Type (X : System.Address)
      with Inline;

   procedure Collect;

   procedure Print_Stats;

end AGC;
