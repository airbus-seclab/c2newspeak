-- Try to call a procedure with too many arguments.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 17 2009
--
procedure t245(X, Y : Integer) is
begin
    t245(1, 2, 3);
end t245;
