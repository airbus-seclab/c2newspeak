-- Constrained float subtype.
--
-- Author : Etienne Millon
-- Date   : Mon Aug 10 2009
--
procedure t362 is
  RMin : constant := 2.0;
  RMax : constant := 5.0;
  subtype R is Float range RMin..Rmax;
  F : R;
begin
  F := 4.0*2.0;
end t362;
