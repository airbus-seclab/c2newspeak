-- 'pos attribute for enumeration types.
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
procedure t328 is
  type T is (A, B);
  X : Integer;
begin
  X := 0;
  X := T'Pos (A);
  X := 1;
  X := T'Pos (B);
end t328;
