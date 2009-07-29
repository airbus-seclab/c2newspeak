-- Make sure that Integer is Standard.Integer.
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
procedure t331 is
  X : Standard.Integer;
  Y : Integer;
begin
    X := Y;
    Y := X;
end t331 ;
