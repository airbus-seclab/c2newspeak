-- Access to pointer types.
--
-- Author : Etienne Millon
-- Date   : Mon Aug  3 2009
--
procedure t348 is
  type P_INT is access INTEGER;
  P : P_INT;
  X : INTEGER;
begin
  X := P.all;
end t348 ;
