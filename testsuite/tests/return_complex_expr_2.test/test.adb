procedure Test is
   type Integer_Access is access Integer;

   type Rec is record
      X : Integer_Access;
   end record;

   function F (X : Integer) return Rec is
   begin
      return Rec'(X => new Integer'(X));
   end F;

   function G (X : Integer) return Integer is
   begin
      return F (X).X.all;
   end G;
begin
   null;
end Test;
