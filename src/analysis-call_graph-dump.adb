with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Dot_Printer;

with Analysis.Allocations;

procedure Analysis.Call_Graph.Dump (Path : String) is
   use Langkit_Support.Text;

   Printer : Dot_Printer.Printer;

   function Split
     (FQN  : Unbounded_Text_Type;
      Name : out Unbounded_Text_Type)
      return Dot_Printer.Cluster_Path_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use type Dot_Printer.Cluster_Path_Type;

      Dot_Index   : constant Natural := Index (FQN, ".");
      Blank_Index : constant Natural := Index (FQN, " ");
   begin
      if Dot_Index = 0 or Blank_Index = 0 or Dot_Index > Blank_Index then
         Name := FQN;
         return Dot_Printer.Empty_Path;
      else
         declare
            Head : constant Unbounded_Text_Type :=
               Unbounded_Slice (FQN, 1, Dot_Index - 1);

            Tail : constant Unbounded_Text_Type :=
               Unbounded_Slice (FQN, Dot_Index + 1, Length (FQN));
         begin
            return (1 => Head) & Split (Tail, Name);
         end;
      end if;
   end Split;

   procedure Process_Summary
     (Caller  : Unbounded_Text_Type;
      Callees : Universal_Solution)
   is
      Id      : constant Ada.Containers.Hash_Type := Wide_Wide_Hash (Caller);

      Name         : Unbounded_Text_Type;
      Cluster_Path : constant Dot_Printer.Cluster_Path_Type :=
         Split (Caller, Name);

      Color     : Unbounded_Text_Type;
      Allocates : Boolean; 
   begin
      if Allocations.Share.Get_Universal_Solution (Caller, Allocates) then
         if Allocates then
            Color := To_Unbounded_Text ("red");
         else
            Color := To_Unbounded_Text ("blue");
         end if;
      end if;

      Printer.Add_Node
        (Id      => Id,
         Name    => Name,
         Cluster => Cluster_Path,
         Color   => Color);

      for C of Callees.Known_Calls loop
         Printer.Add_Edge (Id, Wide_Wide_Hash (C));
      end loop;
   end Process_Summary;
begin
   Call_Graph.Share.Iterate_Universal_Solutions (Process_Summary'Access);
   Printer.Save (Path);
end Analysis.Call_Graph.Dump;
