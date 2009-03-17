-- When calling a subprogram with named parameters, every parameter without
-- a default value shall be in the actual parameter list.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 17 2009
--
procedure t242 (X : Integer ; Y : Integer) is
begin
    t242 (Y => 3);
end t242;
