with Foo;

package Bar is
   type T is tagged null record;

   function Get_U (Self : T) return Foo.U is (null);
end Bar;
