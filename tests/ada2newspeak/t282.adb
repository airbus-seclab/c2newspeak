-- Compile-time modulo operators.
--
-- 4.5.5 : "Signed integer division and remainder are defined by the relation: 
--
--              A = (A/B)*B + (A rem B)
--
--          where (A rem B) has the sign of A and an absolute
--          value less than the absolute value of B.
--          Signed integer division satisfies the identity: 
--
--              (-A)/B = -(A/B) = A/(-B)
--
--          The signed integer modulus operator is defined such that the
--          result of A mod B has the sign of B and an absolute value less
--          than the absolute value of B; in addition, for some signed
--          integer value N, this result satisfies the relation:
--
--             A = B*N + (A mod B)
--         "
--
-- So we have :
--
--   |  A  |  B | A rem B | A mod B |
--   | +12 | +7 |   +5    |   +5    |
--   | +12 | -7 |   +5    |   -2    |
--   | -12 | +7 |   -5    |   +2    |
--   | -12 | -7 |   -5    |   -5    |
--
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
procedure t282 is
  Xp1 : constant := (+12) rem (+7);
  Xp2 : constant := (+12) rem (-7);
  Xm3 : constant := (-12) rem (+7);
  Xm4 : constant := (-12) rem (-7);
  Yp1 : constant := (+12) mod (+7);
  Ym2 : constant := (+12) mod (-7);
  Yp3 : constant := (-12) mod (+7);
  Ym4 : constant := (-12) mod (-7);
  X : Integer;
begin
  X := Xp1;
  X := Xp2;
  X := Xm3;
  X := Xm4;
  X := Yp1;
  X := Ym2;
  X := Yp3;
  X := Ym4;
end t282;
