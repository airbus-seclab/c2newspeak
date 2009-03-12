-- "For .. in .. loop" construct. The iterator is read from inside
-- the "loop" block.
--
-- Author : Etienne Millon
-- Date   : Thu Mar 12 2009
--
procedure t236 is
    X : Integer;
begin
    for I in 5..10 loop
        X := I;
    end loop;
end t236;
