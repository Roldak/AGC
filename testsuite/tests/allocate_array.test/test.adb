procedure Test is
   type Arr is array (Positive range <>) of Integer;

   type Arr_Access is access all Arr;

   X : Arr_Access := new Arr'(1 => 2, 2 => 4);
begin
   AGC.Collect;
end Test;
