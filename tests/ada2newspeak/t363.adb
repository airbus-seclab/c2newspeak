-- 'first and 'last attributes for constrained float subtypes.
--
-- Author : Etienne Millon
-- Date   : Mon Aug 10 2009
--
procedure t363 is
  RMin : constant := 2.0;
  RMax : constant := 5.0;
  subtype R is Float range RMin..Rmax;
  F : R;
begin
  F := 2.0;
  F := R'first;
  F := 5.0;
  F := R'last;
end t363;
