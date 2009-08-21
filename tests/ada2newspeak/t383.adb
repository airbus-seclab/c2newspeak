-- Should not throw "not_found".
--
-- Author : Etienne Millon
-- Date   : Thu Aug 20 2009
--
procedure t383 is
  type T is range 0..1;
  B : Boolean;
begin
  B := T > T;
end t383;
