procedure Test is
   type Quad_Tree;

   type Quad_Tree_Access is access Quad_Tree;

   type Quad_Tree_Access_Array is
      array (Positive range <>) of Quad_Tree_Access;

   type Quad_Tree is record
      Value : Integer;
      Nodes : Quad_Tree_Access_Array (1 .. 4);
   end record;

   X : Quad_Tree_Access;
begin
   declare
   begin
      X := new Quad_Tree'
        (Value => 5,
         Nodes =>
           (1 => null,
            2 => new Quad_Tree'
              (Value => 2,
               Nodes => (null, null, null, null)),
            3 => null,
            4 => null));
   end;
   AGC.Collect;
end Test;
