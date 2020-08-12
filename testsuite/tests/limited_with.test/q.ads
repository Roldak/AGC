with P;

package Q is
   type U is record
      Y : P.Rec;
   end record;

   procedure Foo (X : U);
end Q;
