with GNATCOLL.VFS;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Wide_Wide_Text_IO;

package body Dot_Printer is
   use Ada.Strings.Wide_Wide_Unbounded;

   LF : constant Wide_Wide_String :=
     To_Text ((1 => Ada.Characters.Latin_1.LF));

   function Dot_Concat (X : Cluster_Path_Type) return Unbounded_Text_Type is
     (if X'Length = 0 then Null_Unbounded_Wide_Wide_String
      elsif X'Length = 1 then X (X'First)
      else X (X'First) & "." & Dot_Concat (X (X'First + 1 .. X'Last)));

   function Hex_Byte (X : Natural) return Wide_Wide_String is
      Res : Wide_Wide_String (1 .. 6);
   begin
      Ada.Integer_Wide_Wide_Text_IO.Put (Res, X, 16);
      return Res (4 .. 5);
   end Hex_Byte;

   function Escape (X : Unbounded_Text_Type) return Unbounded_Text_Type is
      Res : Unbounded_Text_Type;
   begin
      for I in 1 .. Length (X) loop
         declare
            C : Wide_Wide_Character := Element (X, I);
         begin
            if C = '"' then
               Append (Res, "\""");
            else
               Append (Res, C);
            end if;
         end;
      end loop;
      return Res;
   end Escape;

   procedure Include_Cluster
     (Cont : in out Cluster_Trees.Tree;
      Root : Cluster_Trees.Cursor;
      Path : Cluster_Path_Type)
   is
      function Get_Or_Append
        (Elem : Unbounded_Text_Type) return Cluster_Trees.Cursor
      is
         use type Cluster_Trees.Cursor;

         Cursor : Cluster_Trees.Cursor := Cluster_Trees.First_Child (Root);
      begin
         while Cursor /= Cluster_Trees.No_Element loop
            if Cluster_Trees.Element (Cursor) = Elem then
               return Cursor;
            end if;
            Cursor := Cluster_Trees.Next_Sibling (Cursor);
         end loop;
         Cont.Append_Child (Root, Elem);
         return Cluster_Trees.Last_Child (Root);
      end Get_Or_Append;
   begin
      if Path'Length = 0 then
         return;
      else
         declare
            Next : constant Cluster_Trees.Cursor :=
               Get_Or_Append (Path (Path'First));
         begin
            Include_Cluster (Cont, Next, Path (Path'First + 1 .. Path'Last));
         end;
      end if;
   end Include_Cluster;

   procedure Add_Node
     (Self    : in out Printer;
      Id      : Node_Id;
      Name    : Unbounded_Text_Type;
      Cluster : Cluster_Path_Type;
      Color   : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String)
   is
      C : Unbounded_Text_Type renames Self.Content;
   begin
      for I in Cluster'Range loop
         Append (C, "subgraph ""cluster_");
         Append (C, Escape (Cluster (I)));
         Append (C, """ {");
         Append (C, LF);
      end loop;

      Include_Cluster (Self.Clusters, Self.Clusters.Root, Cluster);

      Append (C, Id'Wide_Wide_Image);
      Append (C, " [label=""");
      Append (C, Escape (Name));
      Append (C, """");

      if Color /= Null_Unbounded_Wide_Wide_String then
         Append (C, ",color=""");
         Append (C, Color);
         Append (C, """");
      end if;

      Append (C, "];");
      Append (C, LF);

      for I in Cluster'Range loop
         Append (C, "}");
         Append (C, LF);
      end loop;
   end Add_Node;

   procedure Add_Edge
     (Self : in out Printer;
      From : Node_Id;
      To   : Node_Id)
   is
      C : Unbounded_Text_Type renames Self.Content;
   begin
      Append (C, From'Wide_Wide_Image);
      Append (C, " -> ");
      Append (C, To'Wide_Wide_Image);
      Append (C, ";");
      Append (C, LF);
   end Add_Edge;

   procedure Save
     (Self : Printer;
      Path : String)
   is
      use GNATCOLL.VFS;

      C : Unbounded_Text_Type;

      Depth : Natural := 1;

      procedure Process_Cluster (Root : Cluster_Trees.Cursor) is
         Elem : constant Unbounded_Text_Type :=
            Escape (Cluster_Trees.Element (Root));

         Color      : constant Float := 1.0 - 1.0 / Float (Depth + 1);
         Norm_Color : constant Natural := Natural (Color * 255.0);
         Hex_Color  : constant Wide_Wide_String := Hex_Byte (Norm_Color);
      begin
         Append (C, "subgraph ""cluster_");
         Append (C, Elem);
         Append (C, """ {");
         Append (C, LF);
         Append (C, "label=""");
         Append (C, Elem);
         Append (C, """; ");
         Append (C, "style=filled;");
         Append (C, "color=black;");
         Append (C, "fillcolor=""#");
         Append (C, Hex_Color);
         Append (C, Hex_Color);
         Append (C, Hex_Color);
         Append (C, "");
         Append (C, """;");
         Append (C, "fontsize=26;");
         Append (C, LF);

         Depth := Depth + 1;
         Cluster_Trees.Iterate_Children
           (Root, Process_Cluster'Access);
         Depth := Depth - 1;

         Append (C, "}");
         Append (C, LF);
      end Process_Cluster;

      File : Writable_File := Create_From_Base (+Path).Write_File;
   begin
      Cluster_Trees.Iterate_Children
        (Self.Clusters.Root, Process_Cluster'Access);

      Write (File, "digraph g { ");
      Write (File, "rankdir=LR;");
      Write (File, "ranksep=5.0;");
      Write (File, "fontname=Consolas;");
      Write (File, "node [style=filled; color=black; fillcolor=white;];");
      Write (File, Encode (To_Text (Self.Content), "utf-8"));
      Write (File, Encode (To_Text (C), "utf-8"));
      Write (File, "}");
      Close (File);
   end Save;
end Dot_Printer;
