-- Write access to pointer types.
--
-- Author : Etienne Millon
-- Date   : Tue Aug  4 2009
--
procedure t349 is
  type P_INT is access INTEGER;
  P : P_INT;
  X : INTEGER;
begin
  P.all := X;
end t349 ;
