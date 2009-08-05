-- Representation clause : every litteral shall be given a value.
--
-- Author : Etienne Millon
-- Date   : Wed Aug  5 2009
--
procedure t358 is
  type T is (A, B, C);
  for T use (A => 5, B => 6);
begin
    null;
end t358;
