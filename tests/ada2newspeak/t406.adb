-- TRY the same with renaming issue: "subtype T is T405a.T";

with T406a;

procedure t406 is
   subtype TT is T406a.TT;
   X: TT;

begin
   T406a.T(p => X);

end t406;







