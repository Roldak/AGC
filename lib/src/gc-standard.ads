package GC.Standard is
   procedure Visitor_Standard_Integer   is new GC.No_Op (Integer);
   procedure Visitor_Standard_Character is new GC.No_Op (Character);
   procedure Visitor_Standard_String    is new GC.No_Op (String);
end GC.Standard;
