with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access Integer;

   package P is
      type T is tagged private;

      function Create (X : Integer) return T'Class;
      procedure Print (X : T);
   private
      type T is tagged record
         X : Integer_Access;
      end record;
   end P;

   package body P is
      function Create (X : Integer) return T'Class is
      begin
         return T'(X => new Integer'(X));
      end Create;

      procedure Print (X : T) is
      begin
         Put_Line (X.X.all'Image);
      end Print;
   end P;

   package Q is
      type T is new P.T with private;

      function Create (X, Y : Integer) return T'Class;
      overriding procedure Print (X : T);
   private
      type T is new P.T with record
         Y : Integer_Access;
      end record;
   end Q;

   package body Q is
      function Create (X, Y : Integer) return T'Class is
         R : T := (P.T (P.Create (X)) with Y => new Integer'(Y));
      begin
         return R;
      end Create;

      procedure Print (X : T) is
      begin
         P.T (X).Print;
         Put_Line (X.Y.all'Image);
      end Print;
   end Q;

   X : P.T'Class := Q.Create (1, 2);
begin
   X.Print;
end Test;
