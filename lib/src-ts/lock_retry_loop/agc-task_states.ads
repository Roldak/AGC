with AGC.Atomic; use AGC.Atomic;
with AGC.Roots; use AGC.Roots;

package AGC.Task_States is
   type Task_State_Record is tagged private;

   procedure Init (Self : in out Task_State_Record);
   procedure Finalize (Self : in out Task_State_Record);

   procedure Add_Root (Self : in out Task_State_Record; R : Root);
   procedure Pop_Roots (Self : in out Task_State_Record; Count : Natural);
   function Root_Count (Self : Task_State_Record) return Natural;

   procedure Visit_Roots (Self : in out Task_State_Record);

private
   type Task_State_Record is tagged record
      In_Use    : Atomic_Boolean;
      Reach_Set : Root_Vectors.Vector;
   end record;
end AGC.Task_States;
