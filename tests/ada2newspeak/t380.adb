-- '>' for boolean type.
--
-- Author : Etienne Millon
-- Date   : Thu Aug 20 2009
--
procedure t380 is
  type P_Int is access Integer;
  B : Boolean;
  P : P_Int;
begin
  B := P > P;
end t380;
