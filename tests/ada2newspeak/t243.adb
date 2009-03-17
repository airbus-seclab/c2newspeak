-- When calling a subprogram with named parameters,
-- no one shall appear more than once.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 17 2009
--
procedure t243(X : Integer) is
begin
    t243 (X => 3, X => 4);
end t243;
