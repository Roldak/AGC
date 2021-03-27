procedure Test is
   type F_Access is access function (X : Integer) return Integer;

   X : F_Access := null;
begin
   null;
end Test;
