package Analysis.Allocations is
   function Analyze (Subp : LAL.Base_Subp_Body) return Boolean;

   function Default (Subp : LAL.Base_Subp_Body) return Boolean is
     (False);

   function Identity (X : Boolean) return Boolean is (X);

   package Share is new Shared_Analysis
     (Context_Solution   => Boolean,
      Analyze            => Analyze,
      Default            => Default,
      Universal_Solution => Boolean,
      Convert            => Identity);
end Analysis.Allocations;
