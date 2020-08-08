with System;
with System.Storage_Pools; use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;
with System.Memory; use System.Memory;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body AGC.Storage.Free_List is
   Data_Size : constant Storage_Count := 1024 * 1024 * 8;

   Info_Size : constant Storage_Count :=
      Free_Info'Object_Size / System.Storage_Unit;

   type Free_Info_Access is access all Free_Info;
   for Free_Info_Access'Size use Standard'Address_Size;

   function As_Info is new Ada.Unchecked_Conversion
     (System.Address, Free_Info_Access)
         with Inline;

   procedure Collect
     (Self : in out Free_List_Pool; X : System.Address)
   is
      Node : System.Address := X - Info_Size;
      Info : Free_Info renames As_Info (Node).all;
      First : Free_Info renames As_Info (Self.Free).all;
   begin
      First.Prev := Node;
      Info.Next := Self.Free;
      Self.Free := Node;
   end Collect;

   function Is_Valid_Address
     (Self : in out Free_List_Pool; X : System.Address) return Boolean
   is
      use type System.Address;
   begin
      return X >= Self.Data and X < Self.Data + Data_Size;
   end Is_Valid_Address;

   procedure Finalize (Self : in out Free_List_Pool) is
      use type System.Address;
   begin
      if Self.Data /= System.Null_Address then
         Free (Self.Data);
         Self.Data := System.Null_Address;
         Self.Free := System.Null_Address;
      end if;
   end Finalize;

   function Is_Allocated
     (Self : Free_List_Pool;
      Node : System.Address) return Boolean
   is
      use type System.Address;
   begin
      return Node < Self.Data
             or else Node >= Self.Data + Data_Size
             or else (As_Info (Node).Prev = System.Null_Address
                      and then Self.Free /= Node);
   end;

   procedure Init (Self : in out Free_List_Pool) is
   begin
      Self.Data := Alloc (size_t (Data_Size));
      Self.Free := Self.Data;
      As_Info (Self.Free).all :=
        (Size => Data_Size,
         Prev => System.Null_Address,
         Next => System.Null_Address);
   end Init;

   procedure Split
     (Self : in out Free_List_Pool;
      Node : System.Address;
      Required_Size : Storage_Count)
   is
      use type System.Address;

      Info : Free_Info renames As_Info (Node).all;

      Prev_Node : System.Address := Info.Prev;
      Next_Node : System.Address := Info.Next;
   begin
      -- Check if there is enough room to insert a free node in-between.
      -- If not, the whole node will be used for this allocation, using
      -- more space than necessary.
      if Info.Size > Required_Size + Info_Size then
         declare
            New_Node : System.Address := Node + Required_Size;
            New_Info : Free_Info renames As_Info (New_Node).all;
         begin
            New_Info.Size := Info.Size - Required_Size;
            New_Info.Prev := Prev_Node;
            New_Info.Next := Next_Node;

            if Prev_Node = System.Null_Address then
               Self.Free := New_Node;
            else
               As_Info (Prev_Node).Next := New_Node;
            end if;
            if Next_Node /= System.Null_Address then
               As_Info (Next_Node).Prev := New_Node;
            end if;
         end;
         Info.Size := Required_Size;
      else
         if Prev_Node = System.Null_Address then
            Self.Free := Next_Node;
         else
            As_Info (Prev_Node).Next := Next_Node;
         end if;
         if Next_Node /= System.Null_Address then
            As_Info (Next_Node).Prev := Prev_Node;
         end if;
      end if;

      Info.Prev := System.Null_Address;
   end Split;

   procedure Merge
     (Self : in out Free_List_Pool)
   is
      use type System.Address;

      Node : System.Address := Self.Free;
   begin
      while Node /= System.Null_Address loop
         declare
            Info : Free_Info renames As_Info (Node).all;
         begin
            while not Is_Allocated (Self, Node + Info.Size) loop
               declare
                  Consec_Node : System.Address := Node + Info.Size;
                  Consec_Info : Free_Info renames As_Info (Consec_Node).all;
                  Consec_Prev : System.Address := Consec_Info.Prev;
                  Consec_Next : System.Address := Consec_Info.Next;
               begin
                  if Consec_Prev = Node then
                     Info.Next := Consec_Next;
                     if Consec_Next /= System.Null_Address then
                        As_Info (Consec_Next).Prev := Node;
                     end if;
                  elsif Consec_Next = Node then
                     Info.Prev := Consec_Prev;
                     if Consec_Prev /= System.Null_Address then
                        As_Info (Consec_Prev).Next := Node;
                     end if;
                  elsif Consec_Prev = System.Null_Address then
                     As_Info (Info.Prev).Next := Info.Next;
                     if Info.Next /= System.Null_Address then
                        As_Info (Info.Next).Prev := Info.Prev;
                     end if;

                     Info.Next := Consec_Next;
                     if Consec_Next /= System.Null_Address then
                        As_Info (Consec_Next).Prev := Node;
                     end if;

                     Self.Free := Node;
                     Info.Prev := System.Null_Address;
                  elsif Consec_Next = System.Null_Address then
                     As_Info (Consec_Prev).Next := System.Null_Address;
                  else
                     As_Info (Consec_Prev).Next := Consec_Next;
                     As_Info (Consec_Next).Prev := Consec_Prev;
                  end if;
                  Info.Size := Info.Size + Consec_Info.Size;
               end;
            end loop;
            Node := Info.Next;
         end;
      end loop;
   end Merge;

   function Find
     (Self : in out Free_List_Pool;
      Size : Storage_Count) return System.Address
   is
      use type System.Address;

      Required_Size : Storage_Count := Size + Info_Size;

      Free_Node : System.Address := Self.Free;
   begin
      while Free_Node /= System.Null_Address loop
         declare
            Info : Free_Info renames As_Info (Free_Node).all;
         begin
            if Info.Size >= Required_Size then
               Split (Self, Free_Node, Required_Size);
               return Free_Node + Info_Size;
            else
               Free_Node := Info.Next;
            end if;
         end;
      end loop;
      return System.Null_Address;
   end Find;

   procedure Allocate
     (Self : in out Free_List_Pool;
      Addr : out System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
      use type System.Address;
   begin
      if Self.Data = System.Null_Address then
         --  First allocation
         Self.Init;
      end if;
      declare
         Actual_Size : constant Storage_Count := Size + 4;
         Allocated   : System.Address := Find (Self, Actual_Size);
      begin
         if Allocated = System.Null_Address then
            Merge (Self);
            Allocated := Find (Self, Actual_Size);
            if Allocated = System.Null_Address then
               raise Program_Error with "Out of memory";
            end if;
         end if;
         AGC.Register (Allocated, Size);
         Addr := Allocated + 4;
      end;
   end Allocate;

   procedure Deallocate
     (Self : in out Free_List_Pool;
      Addr : System.Address;
      Size : Storage_Count;
      Alignment : Storage_Count)
   is
   begin
      null;
   end Deallocate;
end AGC.Storage.Free_List;
