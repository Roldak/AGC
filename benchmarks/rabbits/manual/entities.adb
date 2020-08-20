package body Entities is
   procedure Delete (E : in out Entity) is
   begin
      E.Alive := False;
   end Delete;
end Entities;
