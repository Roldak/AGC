limited with Q;

package P is
   type U_Access is access Q.U;

   type Rec is record
      X : U_Access;
   end record;

   procedure Bar (X : Rec);
end P;
