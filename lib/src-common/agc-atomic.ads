with Interfaces;

package AGC.Atomic is
   type Atomic_Boolean is tagged private;

   procedure Set (Self : in out Atomic_Boolean; Value : Boolean);
   function Get (Self : Atomic_Boolean) return Boolean;

   function Compare_And_Swap
     (Self  : in out Atomic_Boolean;
      Test  : Boolean;
      Value : Boolean) return Boolean;
   --  If Self contains the ``Test`` value, replace it with ``Value`` and
   --  return True. Otherwise do nothing and return False.

private

   type Atomic_Boolean is tagged record
      Value : aliased Interfaces.Integer_8 := 0;
   end record;
end AGC.Atomic;
