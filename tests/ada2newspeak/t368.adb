-- 'digits.
--
-- Author : Etienne Millon
-- Date   : Mon Aug 17 2009
--
procedure t368 is
  type F1 is digits  6;
  type F2 is digits 11;
  X : Integer;
begin
  X := F1'digits;
  X := F2'digits;
  X := Float'digits;
end t368;
