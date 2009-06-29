-- Ambiguous binary op resolved with second operand resolved at compile-time.
--
-- Author : Etienne Millon
-- Date   : Mon Jun 29 2009
--
procedure t296 is
  type t1 is (A, B);
  type t2 is    (B, C, D);
  X : constant Boolean := (B = C);
  R : Boolean;
begin
  R := X;
end t296 ;
