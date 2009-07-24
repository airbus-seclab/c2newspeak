-- Assignment between a type and one of its subtype.
--
-- Author : Etienne Millon
-- Date   : Fri Jul 24 2009
--
procedure t318 is
  type INT is new INTEGER;
  subtype IRANGE is INT range 0..10;
  X : INT;
  Y : IRANGE;
begin
    X := 5;
    Y := 5;
    X := Y;
    Y := X;
end t318 ;
