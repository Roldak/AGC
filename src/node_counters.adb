package body Node_Counters is
   package LAL     renames Libadalang.Analysis;

   use type Maps.Cursor;

   procedure Increase
     (C      : in out Counter;
      Node   : Libadalang.Analysis.Ada_Node)
   is
      Cursor : Maps.Cursor := Maps.Find (C, Node);
   begin
      if Cursor = Maps.No_element then
         Maps.Insert (C, Node, 1);
      else
         Maps.Replace_Element (C, Cursor, Maps.Element (Cursor) + 1);
      end if;
   end Increase;

   function Get
     (C : Counter; Node : LAL.Ada_Node) return Natural
   is
      Cursor : Maps.Cursor := Maps.Find (C, Node);
   begin
      if Cursor = Maps.No_element then
         return 0;
      else
         return Maps.Element (Cursor);
      end if;
   end Get;

   function Get_Or_Set
     (C     : in out Counter;
      Node  : Libadalang.Analysis.Ada_Node;
      Value : Natural) return Natural
   is
      Cursor : Maps.Cursor := Maps.Find (C, Node);
   begin
      if Cursor = Maps.No_element then
         Maps.Insert (C, Node, Value);
         return Value;
      else
         return Maps.Element (Cursor);
      end if;
   end Get_Or_Set;

   procedure Iterate
     (C : Counter;
      P : not null access procedure (K : LAL.Ada_Node; V : Natural))
   is
      procedure Iter (X : Maps.Cursor) is
      begin
         P (Maps.Key (X), Maps.Element (X));
      end Iter;
   begin
      Maps.Iterate (C, Iter'Access);
   end Iterate;
end Node_Counters;
