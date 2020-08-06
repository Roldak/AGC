with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   package Pkg_T is
      type T is tagged record
         X : Integer_Access;
      end record;

      procedure Print (X : T);
   end Pkg_T;

   package body Pkg_T is
      procedure Print (X : T) is
      begin
         Put_Line (X.X.all'Image);
      end Print;
   end Pkg_T;

   package Pkg_U is
      type U is new Pkg_T.T with record
         Y : Integer_Access;
      end record;
      procedure Print (X : U);
   end Pkg_U;

   package body Pkg_U is
      procedure Print (X : U) is
      begin
         Put_Line (X.X.all'Image & " " & X.Y.all'Image);
      end Print;
   end Pkg_U;

   X : Pkg_T.T'Class := Pkg_U.U'(X => new Integer'(1), Y => new Integer'(2));
begin
   AGC.Collect;
   X.Print;
end Test;
