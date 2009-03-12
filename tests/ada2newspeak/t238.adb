-- "declare" 
--
-- Author : Etienne Millon
-- Date   : Thu Mar 12 2009
--
procedure t238 is
    X,Y : Integer;
begin
    X := 0;
    declare
        Z : Integer;
    begin
        X := 3;
        Z := 8;
    end;
    Y := 5;
end t238;
