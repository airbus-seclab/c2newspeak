-- Hiding declaration.
--
-- Author : Etienne Millon
-- Date   : Thu Jun 18 2009
--
procedure t290 is
  X : Integer;
  Y : Integer;
begin
  declare
    X : Integer;
  begin
    Y := X;
  end;
end t290;
