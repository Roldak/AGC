package body AGC.Task_States is
   procedure Lock (Self : in out Task_State_Record) is
   begin
      loop
         exit when Self.In_Use.Compare_And_Swap (False, True);
      end loop;
   end Lock;

   procedure Unlock (Self : in out Task_State_Record) is
   begin
      Self.In_Use.Set (False);
   end Unlock;

   procedure Init (Self : in out Task_State_Record) is
   begin
      Self.Reach_Set.Reserve (10);
   end Init;

   procedure Finalize (Self : in out Task_State_Record) is
   begin
      Self.Reach_Set.Destroy;
   end Finalize;

   procedure Add_Root (Self : in out Task_State_Record; R : Root) is
   begin
      Lock (Self);
      Self.Reach_Set.Append (R);
      Unlock (Self);
   exception
      when others =>
         Unlock (Self);
         raise;
   end Add_Root;

   procedure Pop_Roots (Self : in out Task_State_Record; Count : Natural) is
   begin
      Lock (Self);
      Self.Reach_Set.Set_Length (Count);
      Unlock (Self);
   exception
      when others =>
         Unlock (Self);
         raise;
   end Pop_Roots;

   function Root_Count (Self : Task_State_Record) return Natural is
   begin
      return Self.Reach_Set.Length;
   end Root_Count;

   procedure Visit_Roots (Self : in out Task_State_Record) is
   begin
      Lock (Self);
      for Root of Self.Reach_Set loop
         As_Address_Visitor (Root.Visitor).all (Root.Addr);
      end loop;
      Unlock (Self);
   exception
      when others =>
         Unlock (Self);
         raise;
   end Visit_Roots;
end AGC.Task_States;

