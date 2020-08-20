package body Entities.Positioned is
   procedure Initialize
     (E : in out Positioned; X, Y : Natural)
   is
   begin
      E.P_X := X;
      E.P_Y := Y;
   end Initialize;

   function X (P : Positioned) return Natural is
   begin
      return P.P_X;
   end X;

   function Y (P : Positioned) return Natural is
   begin
      return P.P_Y;
   end Y;

   procedure Relocate
     (P : aliased in out Positioned; W : in out World;
      New_X, New_Y : Natural)
   is
      Old_X : Natural := P.P_X;
      Old_Y : Natural := P.P_Y;
   begin
      P.P_X := New_X;
      P.P_Y := New_Y;
      W.Move (P'Unchecked_Access, Old_X, Old_Y);
   end Relocate;
end Entities.Positioned;
