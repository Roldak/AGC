package body AGC.Task_States is
   protected body Task_State_Record is
      procedure Init is
      begin
         Reach_Set.Reserve (10);
      end Init;

      procedure Finalize is
      begin
         Reach_Set.Destroy;
      end Finalize;

      procedure Add_Root (R : Root) is
      begin
         Reach_Set.Append (R);
      end Add_Root;

      procedure Pop_Roots (Count : Natural) is
      begin
         Reach_Set.Set_Length (Count);
      end Pop_Roots;

      function Root_Count return Natural is
      begin
         return Reach_Set.Length;
      end Root_Count;

      procedure Visit_Roots is
      begin
         for Root of Reach_Set loop
            As_Address_Visitor (Root.Visitor).all (Root.Addr);
         end loop;
      end Visit_Roots;
   end Task_State_Record;
end AGC.Task_States;

