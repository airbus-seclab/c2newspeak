-- Two representation clauses for the same enumeration.
--
-- Author : Etienne Millon
-- Date   : Wed Aug  5 2009
--
procedure t357 is
  type T is (A, B, C);
  for T use (A => 5, B => 10, C => 15);
  for T use (A => 6, B => 11, C => 16);
begin
    null;
end t357;
