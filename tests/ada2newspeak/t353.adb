-- Access to record fields.
--
-- Author : Etienne Millon
-- Date   : Tue Aug  4 2009
--
procedure t353 is
  type POINT is record
    X : FLOAT;
    Y : FLOAT;
  end record;
  P : POINT;
begin
  P.X := 2.0;
  P.Y := 5.0;
end t353;
