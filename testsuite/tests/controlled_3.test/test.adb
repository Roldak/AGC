with AGC;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Finalization;

procedure Test is
   package Int_Vectors is new Ada.Containers.Vectors
     (Positive, Integer);

   package Pkg is
      type My_Rec is new Ada.Finalization.Controlled with record
         V : Int_Vectors.Vector;
         L : Integer;
      end record;

      overriding procedure Finalize (X : in out My_Rec);
   end Pkg;

   package body Pkg is
      overriding procedure Finalize (X : in out My_Rec) is
      begin
         Put_Line ("My_Rec being finalized");
      end Finalize;
   end Pkg;

   use Pkg;

   procedure Main is
      type My_Rec_Access is access My_Rec;

      X : My_Rec_Access := new My_Rec'
        (Ada.Finalization.Controlled with L => 42, others => <>);
   begin
      null;
   end Main;

   procedure Fill_Stack is
      A : Integer := 42;
      B : Integer := 42;
      C : Integer := 42;
      D : Integer := 42;
      E : Integer := 42;
   begin
      null;
   end Fill_Stack;
begin
   Main;
   Fill_Stack;
   Put_Line ("Before collection");
   AGC.Collect;
end Test;
