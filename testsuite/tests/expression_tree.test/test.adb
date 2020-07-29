with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Expr_Kind is (Add, Mul, Lit, Var);
   type Expr;
   type Expr_Access is access all Expr;
   type Expr (K : Expr_Kind) is record
      case K is
         when Add | Mul =>
            Lhs : Expr_Access;
            Rhs : Expr_Access;
         when Lit =>
            Value : Integer;
         when Var =>
            Name : Character;
      end case;
   end record;

   type Env is array (Character) of Integer;

   function Is_Lit (Ex : Expr_Access; V : Integer) return Boolean is
   begin
      return Ex.K = Lit and then Ex.Value = V;
   end Is_Lit;

   function Simplify (Ex : Expr_Access) return Expr_Access is
   begin
      case Ex.K is
         when Add =>
            if Is_Lit (Ex.Lhs, 0) then
               return Simplify (Ex.Rhs);
            elsif Is_Lit (Ex.Rhs, 0) then
               return Simplify (Ex.Lhs);
            else
               return new Expr'
                 (Add,
                  Simplify (Ex.Lhs),
                  Simplify (Ex.Rhs));
            end if;
         when Mul =>
            if Is_Lit (Ex.Lhs, 0) or else Is_Lit (Ex.Rhs, 0) then
               return new Expr'(Lit, 0);
            elsif Is_Lit (Ex.Lhs, 1) then
               return Simplify (Ex.Rhs);
            elsif Is_Lit (Ex.Rhs, 1) then
               return Simplify (Ex.Lhs);
            else
               return new Expr'
                 (Mul,
                  Simplify (Ex.Lhs),
                  Simplify (Ex.Rhs));
            end if;
         when Lit =>
            return Ex;
         when Var =>
            return Ex;
      end case;
   end Simplify;

   function To_String (Ex : Expr_Access) return String is
   begin
      return
        (case Ex.K is
         when Add =>
            "(" & To_String (Ex.Lhs) & " + " & To_String (Ex.Rhs) & ")",
         when Mul =>
            "(" & To_String (Ex.Lhs) & " * " & To_String (Ex.Rhs) & ")",
         when Lit =>
            Ex.Value'Image,
         when Var =>
            Ex.Name'Image);
   end To_String;

   procedure Main is
      X : Expr_Access := new Expr'
        (Add,
         new Expr'(Lit, 0),
         new Expr'(Mul, new Expr'(Var, 'x'), new Expr'(Lit, 1)));
   begin
      Put_Line (To_String (X));
      Put_Line (To_String (Simplify (X)));
   end Main;
begin
   Main;
   GC.Collect;
end Test;
