with AGC;
with Ada.Containers.Hashed_Maps;

procedure Test is
   type Integer_Access is access Integer;

   package Pkg is
      type Rec is record
         Value : Integer_Access;
      end record;

      function Hash (X : Rec) return Ada.Containers.Hash_Type is
        (Ada.Containers.Hash_Type (X.Value.all));

      function "=" (X, Y : Rec) return Boolean is
        (X.Value.all = Y.Value.all);

      package Int_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type => Rec,
         Element_Type => Rec,
         Hash => Hash,
         Equivalent_Keys => "=",
         "=" => "=");
   end Pkg;

   procedure Main is
      use Pkg;

      M : Int_Maps.Map;
   begin
      M.Insert ((Value => new Integer'(1)), (Value => new Integer'(2)));
      M.Insert ((Value => new Integer'(3)), (Value => new Integer'(4)));
      M.Insert ((Value => new Integer'(5)), (Value => new Integer'(6)));
      AGC.Collect;
      Int_Maps.Element (M.Find ((Value => new Integer'(3)))).Value.all :=
         Int_Maps.Element (M.Find ((Value => new Integer'(5)))).Value.all;
   end Main;
begin
   Main;
   AGC.Collect;
end Test;
