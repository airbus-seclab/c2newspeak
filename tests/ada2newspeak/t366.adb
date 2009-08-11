-- Assignment in a matrix.
--
-- Author : Etienne Millon
-- Date   : Mon Aug 10 2009
--
procedure t366 is
  subtype Index is Integer  range 0..9;
  subtype Index2 is Integer range 0..19;
  type A is array(Index, Index2) of Boolean;
  X : A; 
begin
  X (0,  0) := true;
  X (9,  0) := true;
  X (0, 19) := true;
  X (9, 19) := true;
end t366;
