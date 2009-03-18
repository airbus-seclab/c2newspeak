-- Actual parameters for a "in out" parameter should be a variable.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 17 2009
--
procedure t247(X : in out Integer) is
begin
    t247(2);
end t247;
