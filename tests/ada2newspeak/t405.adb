--TODO  TRY the same with renaming issue: "subtype T is T405a.T";
with T405a;

procedure t405 is
   subtype T is T405a.TT;
   X: T;
begin
   T405a.Troc(p => X);

end t405;







