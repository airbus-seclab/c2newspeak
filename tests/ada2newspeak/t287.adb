-- Overload resolution.
--
-- Author : Etienne Millon
-- Date   : Wed Jun 17 2009
--
procedure t287 is
  type T1 is (A,B);
  type T2 is (B,C,D);
  X : T1;
  Y : T2;
begin
  X := B;
  Y := B;
end t287;
