-- Constrained integer subtype.
--
-- Author : Etienne Millon
-- Date   : Mon Aug 10 2009
--
procedure t364 is
  IRMin : constant := 2;
  IRMax : constant := 5;
  subtype IR is Integer range IRMin..IRmax;
  I_F : IR;
begin
  I_F := 4*2;
end t364;
