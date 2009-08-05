-- Enumerated type representation clause for a non-enumerated type.
--
-- Author : Etienne Millon
-- Date   : Wed Aug  5 2009
--
procedure t356 is
  type R is range -10..10;
  X: constant R := 1;
  Y: constant R := 2;
  for R use (
    X => 3,
    Y => 5);
begin
  null;
end t356;
