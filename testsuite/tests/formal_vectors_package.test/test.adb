with Ada.Containers.Vectors;

procedure Test is
   generic
      type T is private;
      with package Vectors is new Ada.Containers.Vectors
         (Element_Type => T, others => <>);
   package Pkg is
      procedure Main;
   end Pkg;

   package body Pkg is
      procedure Main is
         X : Vectors.Vector;
      begin
         null;
      end Main;
   end Pkg;

   package Int_Vectors is new Ada.Containers.Vectors
     (Positive, Integer);

   package Pkg_I is new Pkg (Integer, Int_Vectors);
begin
   Pkg_I.Main;
end Test;
