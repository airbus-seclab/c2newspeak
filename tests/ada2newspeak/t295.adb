-- Compile-time float unary "+".
--
-- Author : Etienne Millon
-- Date   : Mon Jun 29 2009
--
procedure t295 is
  X : constant := +2.0        ; -- context = nothing
  Y : constant := 0.0 + (+1.0); -- context = float expected
  F : Float;
begin
  F := X;
  F := Y;
end t295 ;
