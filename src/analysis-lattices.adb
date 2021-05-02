with Ada.Strings.Unbounded;

package body Analysis.Lattices is
   package body Lattice is
      function "=" (X, Y : T) return Boolean is
        (X <= Y and then Y <= X);

      function "<" (X, Y : T) return Boolean is
        ((X <= Y) and then not (Y <= X));
   end Lattice;

   package body Finite_Sets is
      function Image (X : Sets.Set) return String is
         use Ada.Strings.Unbounded;
         use type Ada.Containers.Count_Type;

         R : Unbounded_String;
         P : Character := '{';
      begin
         if X.Length = 0 then
            return "{}";
         end if;

         for E of X loop
            Append (R, P);
            Append (R, Element_Image (E));
            P := ',';
         end loop;

         Append (R, "}");
         return To_String (R);
      end Image;
   end Finite_Sets;

end Analysis.Lattices;
