-- "Declare/begin/end" blocks. When leaving the block, local
-- variables should go out from the scope.
--
-- Author : Etienne Millon
-- Date   : Fri Mar 13 2009
--
procedure t239 is
    X : Integer;
begin
    X := 23;
    declare
        Y : Integer;
    begin
        Y := 35;
    end;
    X := 23;
    Y := 35;
end t239;
