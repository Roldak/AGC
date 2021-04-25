with AGC;

procedure Test is
   type Int_Access is access Integer;

   task type T is
      entry Continue;
      entry Collect;
   end T;

   task body T is
      X : Int_Access;
   begin
      loop
         X := new Integer'(1);
         select
            accept Continue;
         or
            accept Collect;
            AGC.Collect;
         or
            terminate;
         end select;
      end loop;
   end T;

   type T_Array is array (Positive range <>) of T;

begin
   declare
      A : T_Array (1 .. 10);
   begin
      for I in 1 .. 10 loop
         for X of A loop
            X.Continue;
         end loop;
      end loop;

      for I in 1 .. 10 loop
         A (1).Collect;
         for X of A (2 .. 10) loop
            X.Continue;
         end loop;
      end loop;
   end;

   AGC.Collect;
end Test;
