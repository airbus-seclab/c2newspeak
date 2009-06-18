-- Overload resolution.
--
-- Binary operators : one side is known, not the other one.
--
-- Author : Etienne Millon
-- Date   : Thu Jun 18 2009
--
procedure t289 is
  type t1 is (A,B);
  type t2 is   (B,C,D);
  X : t1;
  Y : t2;
  R : Boolean;
begin
  R := (X = B); -- B is typed as t1
  R := (B = Y); -- B is typed as t2
end t289;
