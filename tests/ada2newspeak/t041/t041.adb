-- import d'une variable
with t041a;
procedure t041 is
   type Enum is new t041a.E2;
begin
   null;
   t041a.Z := t041a.Y;
end t041;
