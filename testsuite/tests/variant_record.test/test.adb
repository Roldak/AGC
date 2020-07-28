procedure Test is
   type Integer_Access is access all Integer;

   type Kind is (A, B, C);

   type Rec (K : Kind) is record
      X : Integer;
      case K is
         when A =>
            null;
         when B =>
            Y : Integer;
         when C =>
            Z : Integer_Access;
      end case;
   end record;

   R_1 : Rec := (A, 1);
   R_2 : Rec := (C, 2, new Integer'(3));
begin
   GC.Collect;
end Test;
