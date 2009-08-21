-- 'rem' operator for non-numeric types.
--
-- Author : Etienne Millon
-- Date   : Thu Aug 20 2009
--
procedure t377 is
  F : Float;
begin
  F := F rem F;
end t377;
