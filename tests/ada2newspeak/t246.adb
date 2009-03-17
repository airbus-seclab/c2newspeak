-- Actual parameters for a "out" parameter should be a variable.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 17 2009
--
procedure t246(X : out Integer) is
begin
    t246(2);
end t246;
