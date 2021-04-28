with Ada.Unchecked_Conversion;

with System;

with AGC.Vectors;

package AGC.Roots is
   use System;

   type Address_Visitor is access procedure (X : Address);

   function As_Address_Visitor is new Ada.Unchecked_Conversion
     (Address, Address_Visitor)
         with Inline;

   type Root is record
      Addr    : Address;
      Visitor : Address;
   end record;

   package Root_Vectors is new AGC.Vectors (Root);
end AGC.Roots;
