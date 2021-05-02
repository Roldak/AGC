package Analysis.Lattices is
   generic
      type T is private;
      with function "or"  (X, Y : T) return T;
      with function "and" (X, Y : T) return T;
      with function "<="  (X, Y : T) return Boolean;
      with function Image (X : T) return String;
   package Lattice is
      function "=" (X, Y : T) return Boolean;
      function "<" (X, Y : T) return Boolean;
   end Lattice;

   generic
      with package Sets is new Ada.Containers.Hashed_Sets (<>);
      with function Element_Image (X : Sets.Element_Type) return String;
   package Finite_Sets is
      function Image (X : Sets.Set) return String;

      package Lattice is new Lattices.Lattice
        (Sets.Set, Sets.Union, Sets.Intersection, Sets.Is_Subset, Image);
   end Finite_Sets;
end Analysis.Lattices;
