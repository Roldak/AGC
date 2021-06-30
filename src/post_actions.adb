with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Rewriting;
with Libadalang.Unit_Files;

with Node_Counters;
with Session;
with Utils;

package body Post_Actions is
   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;
   package Helpers renames Libadalang.Helpers;

   package Body_Maps is new Ada.Containers.Hashed_Maps
     (LAL.Ada_Node, LALRW.Node_Rewriting_Handle,
      LAL.Hash, LAL."=", LALRW."=");

   type Unit_Info is record
      Name : Unbounded_String;
      Root : LALRW.Node_Rewriting_Handle;
      Buff : Unbounded_String;
      From : LAL.Analysis_Unit;
   end record;

   package Unit_Info_Vectors is new Ada.Containers.Vectors
     (Positive, Unit_Info);

   function Actual_Unit
     (Ctx    : LAL.Analysis_Context;
      Origin : LAL.Analysis_Unit) return LAL.Analysis_Unit
   is (Ctx.Get_From_File (LAL.Get_Filename (Origin)));

   function Lookup
     (Ctx    : LAL.Analysis_Context;
      Origin : LAL.Analysis_Unit;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      F_Kind : LALCO.Ada_Node_Kind_Type := LALCO.Ada_Basic_Decl'First;
      T_Kind : LALCO.Ada_Node_Kind_Type := LALCO.Ada_Basic_Decl'Last)
      return LAL.Ada_Node
   is
      use Langkit_Support.Slocs;

      Unit : LAL.Analysis_Unit := Actual_Unit (Ctx, Origin);
      Node : LAL.Ada_Node      := LAL.Lookup (Unit.Root, Sloc);
   begin
      while Node.Kind not in F_Kind .. T_Kind loop
         Node := Node.Parent;
      end loop;
      return Node;
   end Lookup;

   Body_Map : Body_Maps.Map;
   Created_Units : Unit_Info_Vectors.Vector;

   function Find_Or_Create_Destination
     (Node : LAL.Basic_Decl'Class;
      RH   : LALRW.Rewriting_Handle) return LALRW.Node_Rewriting_Handle;

   function Create_Body_For
     (Pkg : LAL.Base_Package_Decl;
      RH  : LALRW.Rewriting_Handle) return LALRW.Node_Rewriting_Handle
   is
      Ctx : LAL.Analysis_Context := Pkg.Unit.Context;

      Pkg_Node : LAL.Basic_Decl :=
        (if Pkg.Kind in LALCO.Ada_Generic_Package_Internal
         then Pkg.Parent.As_Basic_Decl
         else Pkg.As_Basic_Decl);

      Is_Library_Level : Boolean :=
         Pkg_Node.Parent.Parent.Kind in LALCO.Ada_Compilation_Unit;

      Allows_Body : Boolean :=
         Pkg_Node.P_Semantic_Parent.Kind not in LALCO.Ada_Base_Package_Decl;

      Name : Langkit_Support.Text.Text_Type :=
         LAL.Text (Pkg.F_Package_Name);

      New_Body : LALRW.Node_Rewriting_Handle :=
         LALRW.Create_From_Template
           (RH, "package body " & Name & " is end " & Name & ";",
            (1 .. 0 => <>), LALCO.Package_Body_Rule);
   begin
      if Is_Library_Level then
         declare
            Unit_Name : String := Libadalang.Unit_Files.File_From_Unit
              (Pkg.P_Fully_Qualified_Name, LALCO.Unit_Body);
         begin
            Created_Units.Append
              ((Name => To_Unbounded_String (Unit_name),
                Root => New_Body,
                Buff => <>,
                From => Pkg.Unit));
         end;
      elsif Allows_Body then
         LALRW.Insert_Child
           (LALRW.Handle (Pkg_Node.Parent),
            Utils.Child_Index (LALRW.Handle (Pkg_Node)) + 1,
            New_Body);
      else
         LALRW.Insert_Child
           (Find_Or_Create_Destination (Pkg_Node, RH), 1, New_Body);
      end if;
      return New_Body;
   end Create_Body_For;

   function Find_Or_Create_Destination
     (Node : LAL.Basic_Decl'Class;
      RH   : LALRW.Rewriting_Handle) return LALRW.Node_Rewriting_Handle
   is
      use type Body_Maps.Cursor;

      GGP      : LAL.Ada_Node'Class    := Node.Parent.Parent.Parent;
      Pkg      : LAL.Base_Package_Decl := GGP.As_Base_Package_Decl;
      Cursor   : Body_Maps.Cursor      := Body_Map.Find (Pkg.As_Ada_Node);

      BH : LALRW.Node_Rewriting_Handle;
   begin
      if Cursor /= Body_Maps.No_Element then
         BH := Body_Maps.Element (Cursor);
      else
         declare
            Pkg_Body : LAL.Package_Body := Pkg.P_Body_Part;
         begin
            if Pkg_Body.Is_Null then
               BH := Create_Body_For (Pkg, RH);
            else
               BH := LALRW.Handle (Pkg_Body);
            end if;
         end;
         Body_Map.Insert (Pkg.As_Ada_Node, BH);
      end if;
      return LALRW.Child (LALRW.Child (BH, 3), 1);
   end;

   Insertions : Node_Counters.Counter;

   protected body Actions is
      procedure Move (Action : Move_Action) is
      begin
         To_Move.Append (Action);
      end Move;

      procedure Add_With_Clause (Action : Add_With_Clause_Action) is
      begin
         To_With.Append (Action);
      end Add_With_Clause;

      procedure Add_Generic_Formal (Action : Add_Generic_Formal_Action) is
      begin
         Formals_To_Add.Append (Action);
      end Add_Generic_Formal;

      procedure Add_Generic_Actual (Action : Add_Generic_Actual_Action) is
      begin
         Actuals_To_Add.Append (Action);
      end Add_Generic_Actual;

      procedure Add_Basic_Decl_After (Action : Add_Basic_Decl_After_Action) is
      begin
         Decls_To_Add.Append (Action);
      end Add_Basic_Decl_After;

      procedure Generate_External_Interface
        (Action : Generate_External_Interface_Action)
      is
      begin
         To_Generate.Append (Action);
      end Generate_External_Interface;

      procedure Perform_Actions
        (Ctx   : Analysis_Context;
         Units : in out Helpers.Unit_Vectors.Vector)
      is
         use type LALRW.Node_Rewriting_Handle;

         RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Ctx);
      begin
         for Action of To_Move loop
            declare
               Source : LAL.Basic_Decl :=
                  Lookup (Ctx, Action.Unit, Action.Sloc).As_Basic_Decl;

               Dest   : LALRW.Node_Rewriting_Handle :=
                  Find_Or_Create_Destination (Source, RH);

               Dest_Node : LAL.Ada_Node := LALRW.Node (Dest);
            begin
               if Dest /= LALRW.No_Node_Rewriting_Handle then
                  LALRW.Remove_Child
                    (LALRW.Handle (Source.Parent),
                     Utils.Child_Index (LALRW.Handle (Source)));

                  if Dest_Node.Is_Null then
                     LALRW.Append_Child (Dest, LALRW.Handle (Source));
                  else
                     LALRW.Insert_Child
                       (Dest,
                        Node_Counters.Get (Insertions, Dest_Node) + 1,
                        LALRW.Handle (Source));

                     Node_Counters.Increase (Insertions, Dest_Node);
                  end if;
               end if;
            end;
         end loop;

         for Action of To_Generate loop
            declare
               Typ : LAL.Base_Type_Decl :=
                  Lookup (Ctx, Action.Unit, Action.Sloc).As_Base_Type_Decl;

               Name : Langkit_Support.Text.Text_Type :=
                  Utils.Visitor_Name (Typ);

               Dest : LALRW.Node_Rewriting_Handle := LALRW.Create_From_Template
                 (RH,
                  "with System;"
                  & "procedure " & Name & " (X : System.Address) is "
                  & "begin null; end " & Name & ";",
                  (1 .. 0 => <>),
                  LALCO.Compilation_Rule);

               Unit_Name : String := Libadalang.Unit_Files.File_From_Unit
                 (Name, LALCO.Unit_Body);

               New_Unit : Unit_Info :=
                 (Name => To_Unbounded_String (Unit_Name),
                  Root => Dest,
                  Buff => <>,
                  From => Action.From);
            begin
               Created_Units.Append (New_Unit);
            end;
         end loop;

         for Action of To_With loop
            declare
               Comp_Unit : LAL.Compilation_Unit :=
                  Actual_Unit (Ctx, Action.Unit).Root.As_Compilation_Unit;
            begin
               LALRW.Append_Child
                 (LALRW.Handle (LAL.F_Prelude (Comp_Unit)),
                  LALRW.Create_From_Template
                    (RH,
                     "with " & Langkit_Support.Text.To_Text (Action.Ref) & ";",
                     (1 .. 0 => <>),
                     LALCO.With_Clause_Rule));
            end;
         end loop;

         for Action of Formals_To_Add loop
            declare
               use Langkit_Support.Text;

               Formal_Part : LAL.Generic_Formal_Part := Lookup
                 (Ctx, Action.Unit, Action.Sloc,
                  LALCO.Ada_Generic_Formal_Part,
                  LALCO.Ada_Generic_Formal_Part).As_Generic_Formal_Part;
            begin
               LALRW.Append_Child
                 (LALRW.Handle (Formal_part.F_Decls),
                  LALRW.Create_From_Template
                    (RH, To_Text (Action.Fix), (1 .. 0 => <>),
                     LALCO.Generic_Formal_Decl_Rule));
            end;
         end loop;

         for Action of Actuals_To_Add loop
            declare
               use Langkit_Support.Text;

               Assocs : LAL.Assoc_List := Lookup
                 (Ctx, Action.Unit, Action.Sloc,
                  LALCO.Ada_Assoc_List, LALCO.Ada_Assoc_List).As_Assoc_List;
            begin
               LALRW.Append_Child
                 (LALRW.Handle (Assocs),
                  LALRW.Create_From_Template
                    (RH, To_Text (Action.Fix), (1 .. 0 => <>),
                     LALCO.Param_Assoc_Rule));
            end;
         end loop;

         for Action of Decls_To_Add loop
            declare
               use Langkit_Support.Text;

               Decl : LAL.Basic_Decl := Lookup
                 (Ctx, Action.Unit, Action.Sloc).As_Basic_Decl;

               DH : LALRW.Node_Rewriting_Handle := LALRW.Handle (Decl);
            begin
               LALRW.Insert_Child
                 (LALRW.Parent (DH),
                  Utils.Child_Index (DH) + 1,
                  LALRW.Create_From_Template
                    (RH, To_Text (Action.Fix), (1 .. 0 => <>),
                     LALCO.Basic_Decl_Rule));
            end;
         end loop;

         for Unit_Info of Created_Units loop
            Unit_Info.Buff := To_Unbounded_String
              (Langkit_Support.Text.Encode
                 (LALRW.Unparse (Unit_Info.Root), "utf-8"));
         end loop;

         if not LALRW.Apply (RH).Success then
            raise Program_Error
               with "post_actions: could not apply rewritings";
         end if;

         for Unit_Info of Created_Units loop
            declare
               Name     : String := To_String (Unit_Info.Name);
               Filename : String :=
                  Session.Include_New_Unit (Unit_Info.From, Name);

               Unit : LAL.Analysis_Unit :=
                  LAL.Get_From_Buffer
                    (Ctx,
                     Filename => Filename,
                     Buffer   => Unit_Info.Buff);
            begin
               Units.Append (Unit);
            end;
         end loop;
      end Perform_Actions;

      procedure Clear is
      begin
         To_Move        := Move_Actions.Empty_Vector;
         To_Generate    := Generate_External_Interface_Actions.Empty_Vector;
         To_With        := Add_With_Clause_Actions.Empty_Vector;
         Formals_To_Add := Add_Generic_Formal_Actions.Empty_Vector;
         Actuals_To_Add := Add_Generic_Actual_Actions.Empty_Vector;
         Decls_To_Add   := Add_Basic_Decl_Actions.Empty_Vector;
      end Clear;

      function Length return Natural is
         use type Ada.Containers.Count_Type;

         Total : Ada.Containers.Count_Type := 0;
      begin
         Total := Total + To_Move.Length;
         Total := Total + To_Generate.Length;
         Total := Total + To_With.Length;
         Total := Total + Formals_To_Add.Length;
         Total := Total + Actuals_To_Add.Length;
         Total := Total + Decls_To_Add.Length;
         return Natural (Total);
      end Length;
   end Actions;
end Post_Actions;
