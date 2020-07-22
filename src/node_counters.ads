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
end Node_Counters;
