procedure Test is
   type Int_Access is access Integer;

   function Foo (X : Integer) return Int_Access is
   begin
      case X is
         when 1 =>
            return new Integer'(1);
         when 2 =>
            return new Integer'(2);
         when others =>
            return new Integer'(X - 1);
      end case;
   end Foo;

   function Bar (X : Integer) return Int_Access is
   begin
      case X is
         when 1 =>
            return new Integer'(1);
         when 2 =>
            return new Integer'(2);
         when others =>
            return null;
      end case;
   end Bar;

   A : Int_Access := Foo (2);
   X : Integer := (if A = null then 0 else A.all);

   B : Int_Access := Bar (2);
   Y : Integer := (if B = null then 0 else B.all);
begin
   null;
end Test;
