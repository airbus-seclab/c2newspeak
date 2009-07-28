-- Test that "belongs" checks are made against index subtype
-- and not against its base type.
--
-- Author : Etienne Millon
-- Date   : Tue Jul 28 2009
--
procedure t325 is
  subtype INDEX is INTEGER range 1..10;
  type FLOAT_ARRAY is array (INDEX) of FLOAT;
  A : FLOAT_ARRAY;
  F : FLOAT;
  X : INTEGER;
begin
  F := A (2 * X);
end t325 ;
