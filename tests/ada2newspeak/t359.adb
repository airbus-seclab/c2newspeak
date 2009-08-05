-- For T'Size use...
--
-- Author : Etienne Millon
-- Date   : Wed Aug  5 2009
--
procedure t359 is
  type T is range -2..2;
  for T'Size use 8;
begin
    null;
end t359;
