-- import et use d'une variable et d'un type externe
-- avec conflit possible pour certains symboles (Y et F)
with t042a; use t042a;
procedure t042 is
   type Enum is new E2;
   A : Enum;
begin
   null;
   Z := Y;
   A := F;
end t042;
