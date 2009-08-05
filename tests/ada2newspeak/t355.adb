-- Overloading within a record aggregate.
--
-- Author : Etienne Millon
-- Date   : Wed Aug  5 2009
--
procedure t355 is
  type E1 is (A, B, C);
  type E2 is (   B, C);
  type R is record
    X : E1;
    Y : E2;
  end record;
  T : R;
begin
  T := ( X => B ,  -- 1
         Y => B ); -- 0
end t355;
