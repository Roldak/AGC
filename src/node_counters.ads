with Ada.Containers.Hashed_Maps;

with Libadalang.Analysis;
with Libadalang.Common;

package Node_Counters is
   package Maps is new Ada.Containers.Hashed_Maps
     (Libadalang.Analysis.Ada_Node,
      Natural,
      Libadalang.Analysis.Hash,
      Libadalang.Analysis."=");

   subtype Counter is Maps.Map;

   procedure Increase
     (C : in out Counter; Node : Libadalang.Analysis.Ada_Node);

   function Get
     (C : Counter; Node : Libadalang.Analysis.Ada_Node) return Natural;

   function Get_Or_Set
     (C     : in out Counter;
      Node  : Libadalang.Analysis.Ada_Node;
      Value : Natural) return Natural;

   procedure Iterate
     (C : Counter;
      P : not null access procedure
            (K : Libadalang.Analysis.Ada_Node; V : Natural));
end Node_Counters;
