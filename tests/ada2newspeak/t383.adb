-- Should not throw "not_found".
--
-- Author : Etienne Millon
-- Date   : Thu Aug 20 2009
--
procedure t383 is
  type PI is access Integer;
  B : Boolean;
begin
  B := PI > PI;
end t383;
