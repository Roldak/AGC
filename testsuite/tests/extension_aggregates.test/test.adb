procedure Test is
   package Base is
      type T is abstract tagged record
         X : Integer := 2;
      end record;
   end Base;

   package Derived is
      type T is new Base.T with record
         Y : Boolean;
      end record;
   end Derived;

   use Base;

   X : Derived.T := (T with Y => True);
begin
   null;
end Test;
