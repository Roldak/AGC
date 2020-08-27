generic
   type T is private;
package Pkg is
   type T_Access is access all T;

   function Create return T_Access;
end Pkg;
