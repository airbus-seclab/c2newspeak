-- Constant value propagation.
--
-- Author : Etienne Millon
-- Date   : Fri Jul 10 2009
--
procedure t305 is
  X : constant := 5;
  Y : constant := X;
  Z : Integer;
begin
    Z := Y;
end t305 ;
