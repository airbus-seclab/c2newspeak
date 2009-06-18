-- Overload resolution.
--
-- When inherited types are not enough and a single
-- expected type is not enough too.
--
-- Here, we have to find a type which contains both B and C (ie, t2).
--
-- Author : Etienne Millon
-- Date   : Thu Jun 18 2009
--
procedure t288 is
  type t1 is (A,B);
  type t2 is   (B,C,D);
  type t3 is     (C,D,E,F);
  X : Boolean;
begin
  X := (B = C);
end t288;
