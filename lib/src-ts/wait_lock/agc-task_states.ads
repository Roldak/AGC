with AGC.Roots; use AGC.Roots;

package AGC.Task_States is
   protected type Task_State_Record is
      procedure Init;
      procedure Finalize;

      procedure Add_Root (R : Root);
      procedure Pop_Roots (Count : Natural);
      function Root_Count return Natural;

      procedure Visit_Roots;
   private
      Reach_Set : Root_Vectors.Vector;
   end Task_State_Record;
end AGC.Task_States;
