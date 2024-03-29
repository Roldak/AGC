with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Task_Attributes;

with GNATCOLL.VFS;
with GNATCOLL.Terminal;

with Langkit_Support.Slocs;
with Langkit_Support.Diagnostics.Output;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Iterators;

with Analysis.Allocations;
with Post_Actions;
with Session;

package body Utils is
   function Is_Access_To_Value_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean
   is (Typ.P_Is_Access_Type
       and then Typ.P_Access_Def.Kind not in LALCO.Ada_Access_To_Subp_Def
       and then Typ.Kind not in LALCO.Ada_Anonymous_Type_Decl);

   function Defined_In_Session (BD : LAL.Basic_Decl'Class) return Boolean is
   begin
      return Session.Is_File_To_Process (LAL.Get_Filename (BD.Unit));
   end Defined_In_Session;

   package Node_Bool_Maps is new Ada.Containers.Hashed_Maps
     (LAL.Ada_Node, Boolean, Cached_Node_Hash, LAL."=", "=");

   package RT_Cache is new Ada.Task_Attributes
     (Node_Bool_Maps.Map, Node_Bool_Maps.Empty_Map);

   function Is_Relevant_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean
   is
      use Libadalang.Iterators;

      Full_Typ : LAL.Base_Type_Decl;

      function Is_Known_Irrelevant return Boolean is
         use type Langkit_Support.Text.Unbounded_Text_Type;

         Unit : LAL.Compilation_Unit := Full_Typ.P_Enclosing_Compilation_Unit;
         FQN  : LAL.Unbounded_Text_Type_Array :=
            Unit.P_Syntactic_Fully_Qualified_Name;
      begin
         if FQN'Length >= 2 then
            if FQN (1) = "ada" then
               if FQN (2) = "exceptions" then
                  return True;
               elsif FQN (2) = "strings" then
                  return True;
               end if;
            end if;
         end if;
         return False;
      end Is_Known_Irrelevant;

      function Is_Relevant_Component
        (N : LAL.Ada_Node) return Boolean
      is
      begin
         if N.Kind in LALCO.Ada_Base_Formal_Param_Decl then
            return Is_Relevant_Type
              (N.As_Base_Formal_Param_Decl.P_Formal_Type);
         end if;
         return False;
      end Is_Relevant_Component;

      function Compute return Boolean is
      begin
         if Is_Access_To_Value_Type (Full_Typ) then
            return Defined_In_Session (Full_Typ) or else
                   Is_Relevant_Type (Full_Typ.P_Accessed_Type);
         elsif Is_Known_Irrelevant then
            return False;
         elsif Full_Typ.P_Is_Record_Type then
            if Full_Typ.P_Is_Tagged_Type and then
               Defined_In_Session (Full_Typ)
            then
               return True;
            else
               if not Find_First
                 (Full_Typ.P_Record_Def.F_Components,
                  Is_Relevant_Component'Access).Is_Null
               then
                  return True;
               else
                  return Is_Relevant_Type (Full_Typ.P_Base_Type);
               end if;
            end if;
         else
            return (Full_Typ.P_Is_Array_Type
                    and then Is_Relevant_Type (Full_Typ.P_Comp_Type))
                or else (Full_Typ.P_Is_Classwide
                         and then Is_Relevant_Type
                           (Full_Typ.Parent.As_Base_Type_Decl))
                or else (Full_Typ.P_Is_Interface_Type
                         and then Defined_In_Session (Full_Typ))
                or else Full_Typ.P_Is_Generic_Formal;
         end if;
      end Compute;

      use type Node_Bool_Maps.Cursor;

      Cursor : Node_Bool_Maps.Cursor;
      Inserted : Boolean;
   begin
      if Typ.Is_Null then
         return False;
      end if;

      Full_Typ := Typ.P_Base_Subtype.P_Full_View;

      Cursor := RT_Cache.Reference.Find (Full_Typ.As_Ada_Node);

      if Cursor /= Node_Bool_Maps.No_Element then
         return Node_Bool_Maps.Element (Cursor);
      end if;

      RT_Cache.Reference.Insert
        (Full_Typ.As_Ada_Node, False, Cursor, Inserted);

      declare
         Res : Boolean := Compute;
      begin
         RT_Cache.Reference.Replace_Element (Cursor, Res);
         return Res;
      end;
   end Is_Relevant_Type;

   function Is_Relevant_Root (Decl : LAL.Object_Decl'Class) return Boolean is
      Type_Expr : LAL.Type_Expr := Decl.P_Type_Expression;
      Type_Decl : LAL.Base_Type_Decl := Type_Expr.P_Designated_Type_Decl;
   begin
      return Is_Relevant_Type (Type_Decl) and then not Is_Alias (Decl);
   end Is_Relevant_Root;

   function Is_Managed (Typ : LAL.Base_Type_Decl'Class) return Boolean is
      SP : LAL.Aspect := Typ.P_Get_Aspect (Storage_Pool_Symbol);
   begin
      if not Session.Is_File_To_Process (LAL.Get_Filename (Typ.Unit)) then
         return False;
      elsif not Typ.P_Is_Access_Type then
         return False;
      elsif Typ.Kind in LALCO.Ada_Anonymous_Type_Decl then
         return False;
      elsif not LAL.Exists (Sp) then
         return True;
      elsif LAL.Text (LAL.Value (Sp)) = "AGC.Storage.Get.Pool" then
         return True;
      else
         return False;
      end if;
   end Is_Managed;

   function Is_Alias (Decl : LAL.Object_Decl'Class) return Boolean is
      Is_Constant : Boolean :=
         Decl.F_Has_Constant.Kind in LALCO.Ada_Constant_Present;
   begin
      if not Decl.F_Renaming_Clause.Is_Null then
         return True;
      elsif Is_Constant then
         if Decl.F_Default_Expr.Is_Null then
            --  public constant declaration with completion in private part
            return True;
         else
            return Utils.Is_Named_Expr (Decl.F_Default_Expr);
         end if;
      end if;
      return False;
   end is_Alias;

   function Find_Scope (N : LAL.Ada_Node'Class) return LAL.Ada_Node'Class
   is
      use type LAL.Ada_Node;
      use all type LAL.Expr;

      Parent       : LAL.Ada_Node := N.Parent;
      Grand_Parent : LAL.Ada_Node :=
        (if Parent.Is_Null then LAL.No_Ada_Node else Parent.Parent);
   begin
      if Grand_Parent.Is_Null then
         return LAL.No_Ada_Node;
      elsif Grand_Parent.Kind in LALCO.Ada_Declarative_Part_Range then
         return N;
      elsif Parent.Kind in LALCO.Ada_Stmt_List then
         return N;
      elsif Parent.Kind in LALCO.Ada_Case_Expr_Alternative then
         return N;
      elsif Parent.Kind in LALCO.Ada_Elsif_Expr_Part then
         return N;
      elsif Parent.Kind in LALCO.Ada_If_Expr
            and then N.Kind not in LALCO.Ada_Elsif_Expr_Part_List
            and then N /= Parent.As_If_Expr.F_Cond_Expr
      then
         return N;
      elsif Parent.Kind in LALCO.Ada_Bin_Op
            and then Parent.As_Bin_Op.F_Op.Kind
               in LALCO.Ada_Op_And_Then | LALCO.Ada_Op_Or_Else
            and then N = Parent.As_Bin_Op.F_Right
      then
         return N;
      else
         return Find_Scope (Parent);
      end if;
   end Find_Scope;

   function Enclosing_Subp_Body
     (N : LAL.Ada_Node'Class) return LAL.Base_Subp_Body
   is
   begin
      if N.Kind in LALCO.Ada_Base_Subp_Body then
         return N.As_Base_Subp_Body;
      elsif not N.Parent.Is_Null then
         return Enclosing_Subp_Body (N.Parent);
      else
         return LAL.No_Base_Subp_Body;
      end if;
   end Enclosing_Subp_Body;

   function Defined_In_Subp
     (Subp : LAL.Body_Node'Class;
      Def  : LAL.Defining_name) return Boolean
   is
      use all type LAL.Ada_Node;

      Enclosing : LAL.Base_Subp_Body := Enclosing_Subp_Body (Def);
   begin
      if Enclosing.Is_Null then
         return False;
      elsif Enclosing = Subp then
         return True;
      else
         declare
            Next : LAL.Defining_Name := Def.P_Next_Part;
         begin
            if Next.Is_Null then
               return False;
            else
               return Defined_In_Subp (Subp, Next);
            end if;
         end;
      end if;
   end Defined_In_Subp;

   function Imported_Units
     (Unit        : LAL.Analysis_Unit;
      All_Visible : Boolean := False) return LAL.Analysis_Unit_Array
   is
      use Langkit_Support.Text;

      Provider : LAL.Unit_Provider_Reference := Unit.Context.Unit_Provider;

      package Analysis_Unit_Vectors is new Ada.Containers.Vectors
        (Positive, LAL.Analysis_Unit, LAL."=");

      Result : Analysis_Unit_Vectors.Vector;

      function Designated_Unit (FQN : Text_Type) return LAL.Analysis_Unit is
         Result : LAL.Analysis_Unit'Class := Provider.Get.Get_Unit
           (Unit.Context, FQN, LALCO.Unit_Specification);
      begin
         if not Result.Root.Is_Null then
            return LAL.Analysis_Unit (Result);
         else
            return LAL.Analysis_Unit (Provider.Get.Get_Unit
              (Unit.Context, FQN, LALCO.Unit_Body));
         end if;
      end Designated_Unit;

      function Parent_Unit
        (CU : LAL.Compilation_Unit) return LAL.Analysis_Unit
      is
         Is_Body   : Boolean := CU.P_Unit_Kind in LALCO.Unit_Body;

         FQN_Array : LAL.Unbounded_Text_Type_Array :=
            CU.P_Syntactic_Fully_Qualified_Name;

         FQN          : Text_Type :=
            Dot_Concat (FQN_Array);

         FQN_But_Last : Text_Type :=
            Dot_Concat (FQN_Array (FQN_Array'First .. FQN_Array'Last - 1));

         Result : LAL.Analysis_Unit;
      begin
         if Is_Body then
            Result := LAL.Analysis_Unit (Provider.Get.Get_Unit
              (Unit.Context, FQN, LALCO.Unit_Specification));

            if not Result.Root.Is_Null then
               return Result;
            end if;
         end if;
         return Designated_Unit (FQN_But_Last);
      end Parent_Unit;

      procedure Append_From_Compilation_Unit (CU : LAL.Compilation_Unit) is
         Parent : LAL.Analysis_Unit := Parent_Unit (CU);
      begin
         for Node of CU.F_Prelude loop
            if Node.Kind in LALCO.Ada_With_Clause then
               for Pkg of Node.As_With_Clause.F_Packages loop
                  Result.Append (Designated_Unit (LAL.Text (Pkg.As_Name)));
               end loop;
            end if;
         end loop;
         if not Parent.Root.Is_Null then
            Result.Append (Parent);
            if All_Visible then
               for U of Imported_Units (Parent, All_Visible => True) loop
                  Result.Append (U);
               end loop;
            end if;
         else
            Result.Append (CU.P_Standard_Unit);
         end if;
      end Append_From_Compilation_Unit;
   begin
      case Unit.Root.Kind is
         when LALCO.Ada_Compilation_Unit =>
            Append_From_Compilation_Unit (Unit.Root.As_Compilation_Unit);
         when LALCO.Ada_Compilation_Unit_List =>
            for CU of Unit.Root.As_Compilation_Unit_List loop
               Append_From_Compilation_Unit (CU.As_Compilation_Unit);
            end loop;
         when others =>
            null;
      end case;

      return
         Result_Array : LAL.Analysis_Unit_Array (1 .. Natural (Result.Length))
      do
         for I in Result_Array'Range loop
            Result_Array (I) := Result (I);
         end loop;
      end return;
   end Imported_Units;

   function Visible_Units
     (Unit        : LAL.Analysis_Unit) return LAL.Analysis_Unit_Array
   is (Imported_Units (Unit, All_Visible => True));

   function Is_Actual_Expr (Expr : LAL.Expr'Class) return Boolean is
      use LAL;
   begin
      if Expr.Kind in LALCO.Ada_Box_Expr then
         return False;
      elsif Expr.Kind in LALCO.Ada_Name
         and then Expr.As_Name.P_Is_Defining
      then
         return False;
      elsif
         Expr.Kind in LALCO.Ada_Name
         and then Expr.Parent.Kind in LALCO.Ada_Call_Expr
         and then Expr.Parent.As_Call_Expr.F_Name = Expr
      then
         declare
            N : LAL.Name := Expr.As_Name;
            P : LAL.Name := N.Parent.As_Name;
         begin
            if
               N.P_Is_Call
               and then N.P_Called_Subp_Spec = P.P_Called_Subp_Spec
            then
               return False;
            elsif not N.P_Name_Designated_Type.Is_Null then
               return False;
            end if;
         end;
      elsif
         Expr.Kind in LALCO.Ada_Name
         and then Expr.Parent.Kind in LALCO.Ada_Dotted_Name
         and then Expr.Parent.As_Dotted_Name.F_Suffix = Expr
      then
         return False;
      elsif
         Expr.Kind in LALCO.Ada_Qual_Expr
         or else Expr.Parent.Kind in LALCO.Ada_Qual_Expr
      then
         return False;
      end if;
      return True;
   end Is_Actual_Expr;

   function Is_Named_Expr (Expr : LAL.Expr'Class) return Boolean is
      function Handle_Call (Name : LAL.Name) return Boolean is
         Called_Spec : LAL.Base_Formal_Param_Holder :=
            Name.P_Called_Subp_Spec;

         Subp_Body   : LAL.Body_Node;
      begin
         if Name.P_Is_Dispatching_Call then
            return False;
         elsif Called_Spec.Parent.Kind in LALCO.Ada_Enum_Literal_Decl then
            return True;
         elsif Session.Get_Optimization_Level in Session.None then
            return False;
         end if;

         Subp_Body := Get_Body (Called_Spec.Parent.As_Basic_Decl);

         if Subp_Body.Is_Null then
            return False;
         else
            return not Analysis.Allocations.Share.Get_Universal_Solution
              (Subp_Body);
         end if;
      end Handle_Call;
   begin
      case Expr.Kind is
         when LALCO.Ada_Identifier =>
            if Expr.As_Identifier.P_Is_Call then
               return Handle_Call (Expr.As_Name);
            end if;

            declare
               Decl : LAL.Basic_Decl'Class :=
                  Expr.As_Identifier.P_Referenced_Decl;
            begin
               return
                  Decl.Kind in
                     LALCO.Ada_Object_Decl_Range
                   | LALCO.Ada_For_Loop_Var_Decl
                   | LALCO.Ada_Param_Spec
                   | LALCO.Ada_Component_Decl
                   | LALCO.Ada_Enum_Literal_Decl;
            end;

         when LALCO.Ada_Dotted_Name =>
            return Is_Named_Expr (Expr.As_Dotted_Name.F_Suffix);

         when LALCO.Ada_Paren_Expr =>
            return Is_Named_Expr (Expr.As_Paren_Expr.F_Expr);

         when LALCO.Ada_Null_Literal | LALCO.Ada_Num_Literal =>
            return True;

         when LALCO.Ada_Call_Expr =>
            if not Expr.As_Call_Expr.P_Is_Call then
               return True;
            else
               return Handle_Call (Expr.As_Name);
            end if;

         when LALCO.Ada_Aggregate =>
            return True;

         when LALCO.Ada_Explicit_Deref =>
            return True;

         when LALCO.Ada_Attribute_Ref =>
            return Expr.As_Attribute_Ref.P_Is_Access_Attribute;

         when others =>
            return False;
      end case;
   end Is_Named_Expr;

   function Expands_To_Loop (Expr : LAL.Expr'Class) return Boolean is
      function Designates_Multiple_Targets
        (X : LAL.Alternatives_List) return Boolean
      is
      begin
         if X.Is_Null then
            return False;
         elsif X.Children_Count > 1 then
            return True;
         elsif X.Children_Count = 0 then
            return False;
         else
            return X.Child (1).Kind in
               LALCO.Ada_Others_Designator
               | LALCO.Ada_Bin_Op;
         end if;
      end Designates_Multiple_Targets;

      Parent : constant LAL.Ada_Node := Expr.Parent;
   begin
      if Parent.Kind in
            LALCO.Ada_Quantified_Expr
          | LALCO.Ada_Iterated_Assoc
      then
         return True;
      elsif Parent.Kind in LALCO.Ada_Aggregate_Assoc
            and then Designates_Multiple_Targets
              (Parent.As_Aggregate_Assoc.F_Designators)
      then
         return True;
      else
         return False;
      end if;
   end Expands_To_Loop;

   function Get_Body
     (Decl : LAL.Basic_Decl'Class) return LAL.Body_Node
   is
   begin
      return (if Decl.Kind in LALCO.Ada_Body_Node
              then Decl.As_Body_Node
              else Decl.P_Body_Part_For_Decl);
   end Get_Body;

   function Is_Generalized_Access_Type
     (Typ : LAL.Base_Type_Decl'Class) return Boolean
   is
   begin
      if Typ.Kind in LALCO.Ada_Type_Decl then
         declare
            Def : LAL.Type_Def'Class := Typ.As_Type_Decl.F_Type_Def;
         begin
            return
               Def.Kind in LALCO.Ada_Type_Access_Def
               and then Def.As_Type_Access_Def.F_Has_All.Kind
                           in LALCO.Ada_All_Present;
         end;
      end if;
      return False;
   end Is_Generalized_Access_Type;

   function Generate_Type_Reference
     (Typ             : LAL.Base_Type_Decl'Class;
      Referenced_From : LAL.Analysis_Unit := LAL.No_Analysis_Unit)
      return Langkit_Support.Text.Text_Type
   is
      use type LAL.Analysis_Unit;

      Is_Classwide : Boolean := Typ.Kind in LALCO.Ada_Classwide_Type_Decl;

      Actual_Type : LAL.Base_Type_Decl :=
        (if Is_Classwide
         then Typ.Parent.As_Base_Type_Decl
         else Typ.As_Base_Type_Decl);

      Base : Langkit_Support.Text.Text_Type :=
         Actual_Type.P_Fully_Qualified_Name;

      Suffix : Langkit_Support.Text.Text_Type :=
        (if Is_Classwide
         then "'Class"
         else "");
   begin
      --  Add the missing with clause if the referenced typ is currently
      --  not visible from the "Referenced_From" unit.
      if Referenced_From /= LAL.No_Analysis_Unit and then
         Referenced_From /= Typ.Unit and then
         (for all U of Visible_Units (Referenced_From) => U /= Typ.Unit)
      then
         Post_Actions.Actions.Add_With_Clause
           ((Referenced_From,
             Langkit_Support.Text.To_Unbounded_Text
               (Actual_Type
                .P_Enclosing_Compilation_Unit.P_Decl
                .P_Fully_Qualified_Name)));
      end if;
      return Base & Suffix;
   end Generate_Type_Reference;

   function Is_Runtime_Unit (U : LAL.Compilation_Unit) return Boolean is
      use type Langkit_Support.Text.Unbounded_Text_Type;

      FQN : LAL.Unbounded_Text_Type_Array :=
         U.P_Syntactic_Fully_Qualified_Name;
   begin
      return FQN (FQN'First) = Langkit_Support.Text.To_Unbounded_Text ("ada");
   end Is_Runtime_Unit;

   function Fully_Qualified_Decl_Part_Of
     (Node : LAL.Ada_Node'Class) return Langkit_Support.Text.Text_Type
   is
      Base : LAL.Ada_Node'Class :=
        (if Node.Kind in LALCO.Ada_Classwide_Type_Decl
         then Node.Parent
         else Node);
   begin
      return Base.P_Parent_Basic_Decl.P_Fully_Qualified_Name;
   end Fully_Qualified_Decl_Part_Of;

   function Get_Type_Name
     (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type
   is
   begin
      if Typ.Kind in LALCO.Ada_Anonymous_Type_Decl_Range then
         if Typ.P_Is_Access_Type then
            return "AGC_Anon_Access_To_" & Get_Type_Name (Typ.P_Accessed_Type);
         elsif Typ.P_Is_Array_Type then
            raise Program_Error with "Anonymous array types not handled.";
         else
            raise Program_Error with "Unhandled type.";
         end if;
      else
         return LAL.Text (Typ.P_Defining_Name);
      end if;
   end Get_Type_Name;

   function Is_Subtype_Decl (X : LAL.Base_Type_Decl'Class) return Boolean is
     (X.Kind in LALCO.Ada_Subtype_Decl or else
      (X.Kind in LALCO.Ada_Classwide_Type_Decl and then
       X.As_Classwide_Type_Decl.Parent.Kind in LALCO.Ada_Subtype_Decl));

   function Visitor_Package
     (Node  : LAL.Ada_Node'Class;
      First : Boolean := True) return Langkit_Support.Text.Text_Type
   is
      use type LAL.Analysis_Unit;

      E : Langkit_Support.Text.Text_Type := (if First then "" else ".");
   begin
      if Is_Runtime_Unit (Node.P_Enclosing_Compilation_Unit) then
         declare
            Parent : LAL.Basic_Decl := Node.P_Parent_Basic_Decl;
         begin
            if Parent.Unit = Node.P_Standard_Unit then
               return "AGC.Standard" & E;
            elsif Is_Runtime_Unit (Parent.P_Enclosing_Compilation_Unit) then
               return Visitor_Package (Parent, False)
                  & LAL.Text (Parent.P_Defining_Name.P_Relative_Name)
                  & (if First then "_Visitors" & E else "_");
            else
               return Visitor_Package (Parent, False)
                  & "AGC_"
                  & LAL.Text (Parent.P_Defining_Name.P_Relative_Name)
                  & "_Visitors" & E;
            end if;
         end;
      else
         return Fully_Qualified_Decl_Part_Of (Node) & E;
      end if;
   end Visitor_Package;

   function Visitor_Name
     (Typ                 : LAL.Base_Type_Decl'Class;
      Is_Ref              : Boolean           := True;
      Referenced_From     : LAL.Analysis_Unit := LAL.No_Analysis_Unit)
      return Langkit_Support.Text.Text_Type
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         Get_Type_Name (Typ);

      Is_Standard_Type : Boolean :=
         Is_Runtime_Unit (Typ.P_Enclosing_Compilation_Unit);

      function Normalized_Name
        (Typ : LAL.Base_Type_Decl'Class) return Langkit_Support.Text.Text_Type
      is (if    Typ.P_Is_Classwide
          then  Normalized_Name (Typ.Parent.As_Base_Type_Decl) & "_Classwide"
          elsif Typ.P_Is_Private
          then  Type_Name & "_Private"
          else  Type_Name);

      procedure Handle_Reference (Result : Langkit_Support.Text.Text_Type) is
         use type LAL.Analysis_Unit;
      begin
         if not Session.Is_File_To_Process (LAL.Get_Filename (Typ.Unit))
            and then Referenced_From /= LAL.No_Analysis_Unit
            and then not Is_Standard_Type
         then
            Post_Actions.Actions.Generate_External_Interface
              ((Typ.Unit,
                Langkit_Support.Slocs.Start_Sloc (LAL.Sloc_Range (Typ)),
                Referenced_From));

            Post_Actions.Actions.Add_With_Clause
              ((Referenced_From,
                Langkit_Support.Text.To_Unbounded_Text (Result)));
         end if;
      end Handle_Reference;
   begin
      if Is_Relevant_Type (Typ) then
         if Is_Subtype_Decl (Typ) then
            return Visitor_Name (Typ.P_Base_Subtype, Is_Ref, Referenced_From);
         elsif Is_Ref then
            return Result : Langkit_Support.Text.Text_Type :=
               Visitor_Package (Typ) & ".AGC_Visit_" & Normalized_Name (Typ)
            do
               Handle_Reference (Result);
            end return;
         else
            return "AGC_Visit_" & Normalized_Name (Typ);
         end if;
      else
         return "AGC.No_Op";
      end if;
   end Visitor_Name;

   function Register_Name
     (Typ                 : LAL.Base_Type_Decl'Class;
      Is_Ref              : Boolean           := True)
      return Langkit_Support.Text.Text_Type
   is
      Type_Name : Langkit_Support.Text.Text_Type :=
         Get_Type_Name (Typ);
   begin
      if Is_Subtype_Decl (Typ) then
         return Register_Name (Typ.P_Base_Subtype, Is_Ref);
      elsif Is_Ref then
         return Fully_Qualified_Decl_Part_Of (Typ)
                  & ".AGC_Register_" & Type_Name;
      else
         return "AGC_Register_" & Type_Name;
      end if;
   end Register_Name;

   function Child_Index
     (Node : LALRW.Node_Rewriting_Handle) return Natural
   is
      use type LALRW.Node_Rewriting_Handle;

      Parent : LALRW.Node_Rewriting_Handle := LALRW.Parent (Node);
	begin
      for I in 1 .. LALRW.Children_Count (Parent) loop
         if LALRW.Child (Parent, I) = Node then
            return I;
         end if;
      end loop;
		return 0;
   end Child_Index;

   procedure Force_Set_Child
     (Parent : LALRW.Node_Rewriting_Handle;
      Child  : Positive;
      Node   : LALRW.Node_Rewriting_Handle)
   is
      use type LALRW.Node_Rewriting_Handle;
   begin
      if Node /= LALRW.No_Node_Rewriting_Handle and then LALRW.Tied (Node) then
         LALRW.Replace (Node, LALRW.No_Node_Rewriting_Handle);
      end if;

      LALRW.Set_Child (Parent, Child, Node);
   end Force_Set_Child;

   function To_String (X : Integer) return Langkit_Support.Text.Text_Type is
      Img : Langkit_Support.Text.Text_Type := X'Wide_Wide_Image;
   begin
      return Img (Img'First + 1 .. Img'Last);
   end To_String;

   function Starts_With
     (Str, Prefix : Langkit_Support.Text.Text_Type) return Boolean
   is (Str'Length >= Prefix'Length
       and then Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix);

   function Dot_Concat
     (X : LAL.Unbounded_Text_Type_Array) return Langkit_Support.Text.Text_Type
   is
      use Langkit_Support.Text;
   begin
      if X'Length > 1 then
         return To_Text (X (X'First))
            & "." & Dot_Concat (X (X'First + 1 .. X'Last));
      elsif X'Length > 0 then
         return To_Text (X (X'First));
      end if;
      return "";
   end Dot_Concat;

   function Base_Name (Full_Path : String) return String is
      use GNATCOLL.VFS;
   begin
      return +Create (+Full_Path).Base_Name;
   end Base_Name;

   function Cached_Node_Hash
     (N : LAL.Ada_Node) return Ada.Containers.Hash_Type
   is
   begin
      return LAL.Hash (N);
   exception
      when LALCO.Stale_Reference_Error =>
         return Ada.Containers.Hash_Type (0);
   end Cached_Node_Hash;

   procedure Output_Diagnostic
     (Node : LAL.Ada_Node'Class;
      Message : Langkit_Support.Text.Text_Type;
      Kind : Diagnostic_Kind)
   is
      use Langkit_Support.Diagnostics;
      use Langkit_Support.Diagnostics.Output;
      use Langkit_Support.Text;
      use GNATCOLL.Terminal;

      Diag : constant Diagnostic := Create
        (Sloc_Range => Node.Sloc_Range,
         Message    => Message);

      Diag_Styles : constant array (Diagnostic_Kind) of Diagnostic_Style :=
        (Error   => (To_Unbounded_Text ("error"), Red),
         Warning => (To_Unbounded_Text ("warning"), Yellow),
         Note    => (To_Unbounded_Text ("note"), Unchanged));
   begin
      Print_Diagnostic
        (Diag,
         Node.Unit,
         Base_Name (Node.Unit.Get_Filename),
         Diag_Styles (Kind));
   end Output_Diagnostic;

end Utils;
