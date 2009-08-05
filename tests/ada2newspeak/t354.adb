-- Record aggregate with 'others' clause.
--
-- Author : Etienne Millon
-- Date   : Wed Aug  5 2009
--
procedure t354 is
  type POINT is record
    X : FLOAT;
    Y : FLOAT;
    Z : FLOAT;
  end record;
  P : POINT;
begin
  P := (   X   => 2.0,
        others => 5.0);
end t354;
