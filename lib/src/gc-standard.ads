package GC.Standard is
   function  AGC_Register             is new GC.Register (Integer);
   procedure Visitor_Standard_Integer is new GC.No_Op (Integer);
end GC.Standard;
