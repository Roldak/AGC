package Pkg is
   type Integer_Access is access Integer;

   function Foo return Integer_Access is (null);

   Y : Integer := Foo.all;
end Pkg;
