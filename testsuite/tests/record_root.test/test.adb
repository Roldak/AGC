procedure Test is
   type Integer_Access is access Integer;

   type Rec is record
      Value : Integer_Access;
   end record;

   X : Rec := (Value => new Integer'(2));
begin
   AGC.Collect;
   X.Value.all := 3;
end Test;
