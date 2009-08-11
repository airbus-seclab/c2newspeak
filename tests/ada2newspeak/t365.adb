-- Declaration of a matrix.
--
-- Author : Etienne Millon
-- Date   : Mon Aug 10 2009
--
procedure t365 is
  subtype Index is Integer  range 0..9;
  subtype Index2 is Integer range 0..19;
  type A is array(Index, Index2) of Boolean;
  X : A; 
begin
  null;
end t365;
