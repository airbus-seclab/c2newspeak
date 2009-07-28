-- Array of floats : ensures that no confusion is made between component and
-- index type.
--
-- Author : Etienne Millon
-- Date   : Tue Jul 28 2009
--
procedure t324 is
  subtype INDEX is INTEGER range 1..10;
  type FLOAT_ARRAY is array (INDEX) of FLOAT;
  A : FLOAT_ARRAY;
  F : FLOAT;
  X : INTEGER;
begin
    X := 5;
    F := A (X);
end t324 ;
