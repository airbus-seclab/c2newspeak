-- import d'une fonction, visible malgé autre symbole Z
-- use du package
with T047a; use T047a;
procedure T047 is
   type T is (Z,A);
begin
   if Z(42)
   then
      null;
   end if;
end T047;
