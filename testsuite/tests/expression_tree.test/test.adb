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
            elsif Ex.Lhs.K = Lit and Ex.Rhs.K = Lit then
               return new Expr'(Lit, Ex.Lhs.Value + Ex.Rhs.Value);
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
            elsif Ex.Lhs.K = Lit and Ex.Rhs.K = Lit then
               return new Expr'(Lit, Ex.Lhs.Value * Ex.Rhs.Value);
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

   function Substitute (Ex : Expr_Access; Ev : Env) return Expr_Access is
   begin
      case Ex.K is
         when Add =>
            return new Expr'
              (Add,
               Substitute (Ex.Lhs, Ev),
               Substitute (Ex.Rhs, Ev));
         when Mul =>
            return new Expr'
              (Mul,
               Substitute (Ex.Lhs, Ev),
               Substitute (Ex.Rhs, Ev));
         when Lit =>
            return Ex;
         when Var =>
            return new Expr'(Lit, Ev (Ex.Name));
      end case;
   end Substitute;

   function Evaluate (Ex : Expr_Access; Ev : Env) return Integer is
      Simplified : Expr_Access := Simplify (Substitute (Ex, Ev));
   begin
      if Simplified.K = Lit then
         return Simplified.Value;
      else
         return Evaluate (Simplified, Ev);
      end if;
   end Evaluate;

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

      Y : Expr_Access := new Expr'
        (Mul, new Expr'(Lit, 2), new Expr'(Lit, 4));

      Z : Expr_Access := new Expr'(Add, X, Y);

      Ev : Env := ('x' => 42, others => 0);
   begin
      Put_Line (To_String (Z));
      Put_Line (To_String (Simplify (Z)));
      Put_Line (To_String (Substitute (Z, Ev)));
      Put_Line (Evaluate (Z, Ev)'Image);
   end Main;
begin
   Main;
   AGC.Collect;
end Test;
